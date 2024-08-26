let anchor = Unix.gettimeofday ()

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("[%a]%a[%a][%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Cyan (fmt "%.04f"))
        (Unix.gettimeofday () -. anchor)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Blue int)
        (Stdlib.Domain.self () :> int)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stderr)

(* let () = Logs.set_level ~all:true (Some Logs.Debug) *)
let () = Logs_threaded.enable ()
let () = Printexc.record_backtrace true
let domains = 3

let server ?(port = 8080) handler =
  let stop = Httpcats.Server.stop () in
  let prm =
    Miou.call @@ fun () ->
    let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
    Httpcats.Server.clear ~stop ~handler sockaddr
  in
  (stop, prm)

module Ca = struct
  open Rresult

  let prefix =
    X509.Distinguished_name.
      [ Relative_distinguished_name.singleton (CN "HTTPcats") ]

  let cacert_dn =
    X509.Distinguished_name.(
      prefix
      @ [
          Relative_distinguished_name.singleton (CN "Ephemeral CA for httpcats")
        ])

  let cacert_lifetime = Ptime.Span.v (365, 0L)
  let _10s = Ptime.Span.of_int_s 10

  let make domain_name seed =
    Domain_name.of_string domain_name >>= Domain_name.host
    >>= fun domain_name ->
    let private_key =
      let seed = Base64.decode_exn ~pad:false seed in
      let g = Mirage_crypto_rng.(create ~seed (module Fortuna)) in
      Mirage_crypto_pk.Rsa.generate ~g ~bits:2048 ()
    in
    let valid_from =
      Option.get Ptime.(sub_span (v (Ptime_clock.now_d_ps ())) _10s)
    in
    Ptime.add_span valid_from cacert_lifetime
    |> Option.to_result ~none:(R.msgf "End time out of range")
    >>= fun valid_until ->
    X509.Signing_request.create cacert_dn (`RSA private_key) >>= fun ca_csr ->
    let extensions =
      let open X509.Extension in
      let key_id =
        X509.Public_key.id X509.Signing_request.((info ca_csr).public_key)
      in
      empty
      |> add Subject_alt_name
           ( true
           , X509.General_name.(
               singleton DNS [ Domain_name.to_string domain_name ]) )
      |> add Basic_constraints (true, (false, None))
      |> add Key_usage
           (true, [ `Digital_signature; `Content_commitment; `Key_encipherment ])
      |> add Subject_key_id (false, key_id)
    in
    X509.Signing_request.sign ~valid_from ~valid_until ~extensions ca_csr
      (`RSA private_key) cacert_dn
    |> R.reword_error (R.msgf "%a" X509.Validation.pp_signature_error)
    >>= fun certificate ->
    let fingerprint = X509.Certificate.fingerprint `SHA256 certificate in
    let time () = Some (Ptime_clock.now ()) in
    let authenticator =
      X509.Authenticator.cert_fingerprint ~time ~hash:`SHA256 ~fingerprint
    in
    Ok (certificate, `RSA private_key, authenticator)
end

let secure_server ~seed ?(port = 8080) handler =
  let stop = Httpcats.Server.stop () in
  let cert, pk, authenticator =
    Rresult.R.failwith_error_msg (Ca.make "http.cats" seed)
  in
  let prm =
    Miou.async @@ fun () ->
    let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
    let cfg =
      Tls.Config.server
        ~certificates:(`Single ([ cert ], pk))
        ~alpn_protocols:[ "h2"; "http/1.1" ] ()
      |> Result.get_ok
    in
    Httpcats.Server.with_tls ~stop cfg ~handler sockaddr
  in
  (stop, prm, authenticator)

let test00 =
  Alcotest.test_case "simple" `Quick @@ fun () ->
  Miou_unix.run ~domains @@ fun () ->
  let handler = function
    | `V2 _ -> assert false
    | `V1 reqd ->
        let open H1 in
        let body = "Hello World!" in
        let headers =
          Headers.of_list
            [
              ("content-type", "text/plain")
            ; ("content-length", string_of_int (String.length body))
            ]
        in
        let resp = Response.create ~headers `OK in
        Reqd.respond_with_string reqd resp body
  in
  let stop, prm = server ~port:4000 handler in
  let daemon, resolver = Happy_eyeballs_miou_unix.create () in
  match
    Httpcats.request ~resolver
      ~f:(fun _resp buf str -> Buffer.add_string buf str; buf)
      ~uri:"http://127.0.0.1:4000/" (Buffer.create 0x10)
  with
  | Ok (_response, buf) ->
      Alcotest.(check string)
        "Hello World!" (Buffer.contents buf) "Hello World!";
      Httpcats.Server.switch stop;
      Miou.await_exn prm;
      Happy_eyeballs_miou_unix.kill daemon
  | Error err ->
      Httpcats.Server.switch stop;
      Miou.await_exn prm;
      Happy_eyeballs_miou_unix.kill daemon;
      Alcotest.failf "Got an error: %a" Httpcats.pp_error err

let generate g len =
  let result = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set result i (Char.chr (Random.State.bits g land 0xff))
  done;
  Bytes.unsafe_to_string result

let test01 =
  Alcotest.test_case "stream" `Quick @@ fun () ->
  Miou_unix.run ~domains @@ fun () ->
  let g0 = Random.State.make_self_init () in
  let g1 = Random.State.copy g0 in
  let max = 0x100000 in
  let chunk = 0x10 in
  let handler = function
    | `V2 _ -> assert false
    | `V1 reqd ->
        Logs.debug (fun m -> m "Got a request");
        let open H1 in
        let headers =
          Headers.of_list
            [
              ("content-type", "text/plain")
            ; ("content-length", string_of_int max)
            ]
        in
        let resp = Response.create ~headers `OK in
        let body = Reqd.respond_with_streaming reqd resp in
        let rec go rest =
          if rest <= 0 then Body.Writer.close body
          else
            let len = min chunk rest in
            let str = generate g0 len in
            Body.Writer.write_string body str;
            go (rest - len)
        in
        go max
  in
  let stop, prm = server ~port:4000 handler in
  let daemon, resolver = Happy_eyeballs_miou_unix.create () in
  match
    Httpcats.request ~resolver
      ~f:(fun _resp buf str -> Buffer.add_string buf str; buf)
      ~uri:"http://127.0.0.1:4000" (Buffer.create 0x1000)
  with
  | Ok (_response, buf) ->
      Alcotest.(check string) "random" (generate g1 max) (Buffer.contents buf);
      Httpcats.Server.switch stop;
      Miou.await_exn prm;
      Happy_eyeballs_miou_unix.kill daemon
  | Error err ->
      Httpcats.Server.switch stop;
      Miou.await_exn prm;
      Happy_eyeballs_miou_unix.kill daemon;
      Alcotest.failf "Got an error: %a" Httpcats.pp_error err

let sha1 = Alcotest.testable Digestif.SHA1.pp Digestif.SHA1.equal

let random_string ~len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set res i (Char.unsafe_chr (Random.bits () land 0xff))
  done;
  Bytes.unsafe_to_string res

let random_string_seq ?(chunk = 0x100) ~g ~len =
  let tmp = Bytes.create chunk in
  let snd = ref 0 in
  let dispenser () =
    let len = min (len - !snd) (Bytes.length tmp) in
    if len = 0 then None
    else begin
      for i = 0 to len - 1 do
        Bytes.set tmp i (Char.unsafe_chr (Random.State.bits g land 0xff))
      done;
      snd := !snd + len;
      Some (Bytes.sub_string tmp 0 len)
    end
  in
  Seq.of_dispenser dispenser

let fold_http_1_1 ~finally ~f acc body =
  let open H1 in
  let acc = ref acc in
  let rec on_eof () = Body.Reader.close body; finally !acc
  and on_read bstr ~off ~len =
    let str = Bigstringaf.substring bstr ~off ~len in
    Logs.debug (fun m -> m "Feed the context");
    acc := f !acc str;
    Body.Reader.schedule_read body ~on_eof ~on_read
  in
  Body.Reader.schedule_read body ~on_eof ~on_read

let fold_h2 ~finally ~f acc body =
  let open H2 in
  let acc = ref acc in
  let rec on_eof () = Body.Reader.close body; finally !acc
  and on_read bstr ~off ~len =
    let str = Bigstringaf.substring bstr ~off ~len in
    Logs.debug (fun m -> m "Feed the context");
    acc := f !acc str;
    Body.Reader.schedule_read body ~on_eof ~on_read
  in
  Body.Reader.schedule_read body ~on_eof ~on_read

let test02 =
  Alcotest.test_case "post" `Quick @@ fun () ->
  Miou_unix.run ~domains @@ fun () ->
  let handler = function
    | `V2 _ -> assert false
    | `V1 reqd ->
        let open H1 in
        let f ctx str = Digestif.SHA1.feed_string ctx str in
        let finally ctx =
          let hash = Digestif.SHA1.(to_hex (get ctx)) in
          let headers =
            Headers.of_list
              [
                ("content-type", "text/plain")
              ; ("content-length", string_of_int (String.length hash))
              ]
          in
          let resp = Response.create ~headers `OK in
          Reqd.respond_with_string reqd resp hash
        in
        fold_http_1_1 ~finally ~f Digestif.SHA1.empty (Reqd.request_body reqd)
  in
  let stop, prm = server ~port:4000 handler in
  let daemon, resolver = Happy_eyeballs_miou_unix.create () in
  let g0 = Random.State.make_self_init () in
  let body =
    Httpcats.stream (random_string_seq ~g:(Random.State.copy g0) ~len:0x4000)
  in
  match
    Httpcats.request ~resolver ~meth:`POST ~body
      ~f:(fun _resp buf str -> Buffer.add_string buf str; buf)
      ~uri:"http://127.0.0.1:4000" (Buffer.create 0x1000)
  with
  | Ok (_response, buf) ->
      let hash' = Digestif.SHA1.of_hex (Buffer.contents buf) in
      let hash =
        Digestif.SHA1.digesti_string
          ((Fun.flip Seq.iter) (random_string_seq ~g:g0 ~len:0x4000))
      in
      Alcotest.(check sha1) "sha1" hash hash';
      Httpcats.Server.switch stop;
      Miou.await_exn prm;
      Happy_eyeballs_miou_unix.kill daemon
  | Error err ->
      Httpcats.Server.switch stop;
      Miou.await_exn prm;
      Happy_eyeballs_miou_unix.kill daemon;
      Alcotest.failf "Got an error: %a" Httpcats.pp_error err

let seed = Base64.encode_exn "foo"
let ( % ) f g x = f (g x)

let test03 =
  Alcotest.test_case "simple" `Quick @@ fun () ->
  let open Rresult in
  Miou_unix.run ~domains @@ fun () ->
  let handler = function
    | `V2 reqd ->
        let open H2 in
        let body = "Hello World!" in
        let headers =
          Headers.of_list
            [
              ("content-type", "text/plain")
            ; ("content-length", string_of_int (String.length body))
            ]
        in
        let resp = Response.create ~headers `OK in
        Reqd.respond_with_string reqd resp body
    | `V1 reqd ->
        let open H1 in
        let body = "Hello World!" in
        let headers =
          Headers.of_list
            [
              ("content-type", "text/plain")
            ; ("content-length", string_of_int (String.length body))
            ]
        in
        let resp = Response.create ~headers `OK in
        Reqd.respond_with_string reqd resp body
  in
  let stop, prm, authenticator = secure_server ~seed ~port:4000 handler in
  let daemon, resolver = Happy_eyeballs_miou_unix.create () in
  let http_1_1 =
    Miou.async @@ fun () ->
    let tls_config =
      Tls.Config.client ~authenticator ~alpn_protocols:[ "http/1.1" ] ()
      |> Result.get_ok
    in
    Httpcats.request ~resolver ~tls_config
      ~f:(fun _resp buf str -> Buffer.add_string buf str; buf)
      ~uri:"https://127.0.0.1:4000" (Buffer.create 0x10)
    |> R.reword_error (R.msgf "%a" Httpcats.pp_error)
  in
  let h2 =
    Miou.async @@ fun () ->
    Httpcats.request ~resolver ~authenticator
      ~f:(fun _resp buf str -> Buffer.add_string buf str; buf)
      ~uri:"https://127.0.0.1:4000" (Buffer.create 0x10)
    |> R.reword_error (R.msgf "%a" Httpcats.pp_error)
  in
  match
    Miou.await_all [ h2; http_1_1 ]
    |> List.map (R.reword_error (R.msgf "%s" % Printexc.to_string))
    |> List.map Result.join
  with
  | [ Ok (_, buf0); Ok (_, buf1) ] ->
      Alcotest.(check string)
        "Hello World!" (Buffer.contents buf0) "Hello World!";
      Alcotest.(check string)
        "Hello World!" (Buffer.contents buf1) "Hello World!";
      Httpcats.Server.switch stop;
      Miou.await_exn prm;
      Happy_eyeballs_miou_unix.kill daemon
  | _ ->
      Httpcats.Server.switch stop;
      Miou.await_exn prm;
      Happy_eyeballs_miou_unix.kill daemon;
      Alcotest.failf "Unexpected result"

let test04 =
  let open Rresult in
  Alcotest.test_case "stream" `Quick @@ fun () ->
  Miou_unix.run ~domains @@ fun () ->
  let g0 = Random.State.make_self_init () in
  let g1 = Random.State.copy g0 in
  let g2 = Random.State.copy g0 in
  let max = 0x100000 in
  let chunk = 0x10 in
  let handler = function
    | `V2 reqd ->
        let open H2 in
        let headers =
          Headers.of_list
            [
              ("content-type", "text/plain")
            ; ("content-length", string_of_int max)
            ]
        in
        let resp = Response.create ~headers `OK in
        let body = Reqd.respond_with_streaming reqd resp in
        let rec go rest =
          if rest <= 0 then Body.Writer.close body
          else
            let len = min chunk rest in
            let str = generate g2 len in
            Body.Writer.write_string body str;
            go (rest - len)
        in
        go max
    | `V1 reqd ->
        Logs.debug (fun m -> m "Got a request");
        let open H1 in
        let headers =
          Headers.of_list
            [
              ("content-type", "text/plain")
            ; ("content-length", string_of_int max)
            ]
        in
        let resp = Response.create ~headers `OK in
        let body = Reqd.respond_with_streaming reqd resp in
        let rec go rest =
          if rest <= 0 then Body.Writer.close body
          else
            let len = min chunk rest in
            let str = generate g1 len in
            Body.Writer.write_string body str;
            go (rest - len)
        in
        go max
  in
  let stop, prm, authenticator = secure_server ~seed ~port:4000 handler in
  let daemon, resolver = Happy_eyeballs_miou_unix.create () in
  let http_1_1 =
    Miou.async @@ fun () ->
    let tls_config =
      Tls.Config.client ~authenticator ~alpn_protocols:[ "http/1.1" ] ()
      |> Result.get_ok
    in
    Httpcats.request ~resolver ~tls_config
      ~f:(fun _resp buf str -> Buffer.add_string buf str; buf)
      ~uri:"https://127.0.0.1:4000" (Buffer.create 0x1000)
    |> R.reword_error (R.msgf "%a" Httpcats.pp_error)
  in
  let h2 =
    Miou.async @@ fun () ->
    Httpcats.request ~resolver ~authenticator
      ~f:(fun _resp buf str -> Buffer.add_string buf str; buf)
      ~uri:"https://127.0.0.1:4000" (Buffer.create 0x10)
    |> R.reword_error (R.msgf "%a" Httpcats.pp_error)
  in
  match
    Miou.await_all [ h2; http_1_1 ]
    |> List.map (R.reword_error (R.msgf "%s" % Printexc.to_string))
    |> List.map Result.join
  with
  | [ Ok (_, buf0); Ok (_, buf1) ] ->
      Alcotest.(check string)
        "random"
        (generate (Random.State.copy g0) max)
        (Buffer.contents buf0);
      Alcotest.(check string)
        "random"
        (generate (Random.State.copy g0) max)
        (Buffer.contents buf1);
      Httpcats.Server.switch stop;
      Miou.await_exn prm;
      Happy_eyeballs_miou_unix.kill daemon
  | _ ->
      Httpcats.Server.switch stop;
      Miou.await_exn prm;
      Happy_eyeballs_miou_unix.kill daemon;
      Alcotest.failf "Unexpected result"

let test05 =
  let open Rresult in
  Alcotest.test_case "post" `Quick @@ fun () ->
  Miou_unix.run ~domains @@ fun () ->
  let handler = function
    | `V2 reqd ->
        let open H2 in
        let f ctx str = Digestif.SHA1.feed_string ctx str in
        let finally ctx =
          let hash = Digestif.SHA1.(to_hex (get ctx)) in
          let headers =
            Headers.of_list
              [
                ("content-type", "text/plain")
              ; ("content-length", string_of_int (String.length hash))
              ]
          in
          let resp = Response.create ~headers `OK in
          Reqd.respond_with_string reqd resp hash
        in
        fold_h2 ~finally ~f Digestif.SHA1.empty (Reqd.request_body reqd)
    | `V1 reqd ->
        let open H1 in
        let f ctx str = Digestif.SHA1.feed_string ctx str in
        let finally ctx =
          let hash = Digestif.SHA1.(to_hex (get ctx)) in
          let headers =
            Headers.of_list
              [
                ("content-type", "text/plain")
              ; ("content-length", string_of_int (String.length hash))
              ]
          in
          let resp = Response.create ~headers `OK in
          Reqd.respond_with_string reqd resp hash
        in
        fold_http_1_1 ~finally ~f Digestif.SHA1.empty (Reqd.request_body reqd)
  in
  let stop, prm, authenticator = secure_server ~seed ~port:4000 handler in
  let daemon, resolver = Happy_eyeballs_miou_unix.create () in
  let body = random_string ~len:0x4000 in
  let http_1_1 =
    Miou.async @@ fun () ->
    let tls_config =
      Tls.Config.client ~authenticator ~alpn_protocols:[ "http/1.1" ] ()
      |> Result.get_ok
    in
    Httpcats.request ~resolver ~tls_config ~meth:`POST
      ~body:(Httpcats.string body)
      ~f:(fun _resp buf str -> Buffer.add_string buf str; buf)
      ~uri:"https://127.0.0.1:4000" (Buffer.create 0x1000)
    |> R.reword_error (R.msgf "%a" Httpcats.pp_error)
  in
  let h2 =
    Miou.async @@ fun () ->
    Httpcats.request ~resolver ~authenticator ~meth:`POST
      ~body:(Httpcats.string body)
      ~f:(fun _resp buf str -> Buffer.add_string buf str; buf)
      ~uri:"https://127.0.0.1:4000" (Buffer.create 0x1000)
    |> R.reword_error (R.msgf "%a" Httpcats.pp_error)
  in
  match
    Miou.await_all [ h2; http_1_1 ]
    |> List.map (R.reword_error (R.msgf "%s" % Printexc.to_string))
    |> List.map Result.join
  with
  | [ Ok (_, buf0); Ok (_, buf1) ] ->
      let hash0 = Digestif.SHA1.of_hex (Buffer.contents buf0) in
      let hash1 = Digestif.SHA1.of_hex (Buffer.contents buf1) in
      let hash = Digestif.SHA1.digest_string body in
      Alcotest.(check sha1) "sha1" hash hash0;
      Alcotest.(check sha1) "sha1" hash hash1;
      Httpcats.Server.switch stop;
      Miou.await_exn prm;
      Happy_eyeballs_miou_unix.kill daemon
  | _ ->
      Httpcats.Server.switch stop;
      Miou.await_exn prm;
      Happy_eyeballs_miou_unix.kill daemon;
      Alcotest.failf "Unexpected result"

let () =
  let stdout = Alcotest_engine.Formatters.make_stdout () in
  let stderr = Alcotest_engine.Formatters.make_stderr () in
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna);
  Alcotest.run ~stdout ~stderr "network"
    [
      ("clear", [ test00; test01; test02 ])
    ; ("with-tls", [ test03; test04; test05 ])
    ]
