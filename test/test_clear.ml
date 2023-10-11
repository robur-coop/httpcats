let anchor = Unix.gettimeofday ()

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
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
let () = Logs.set_level ~all:true (Some Logs.Debug)
let () = Logs_threaded.enable ()
let () = Printexc.record_backtrace true

let server ?(port = 8080) handler =
  let stop = Atomic.make false in
  let prm =
    Miou.call @@ fun () ->
    let file_descr = Miou_unix.tcpv4 () in
    Miou_unix.bind_and_listen file_descr
      (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
    Httpcats.Server.clear ~stop ~handler file_descr;
    Miou_unix.close file_descr
  in
  (stop, prm)

let test00 =
  Alcotest.test_case "simple" `Quick @@ fun () ->
  Miou_unix.run @@ fun () ->
  let handler _request =
    let open Httpaf in
    let body = "Hello World!" in
    let headers =
      Headers.of_list
        [ ("content-type", "text/plain")
        ; ("content-length", string_of_int (String.length body)) ]
    in
    let response = Response.create ~headers `OK in
    Httpcats.Server.string (`V1 response) body
  in
  let stop, prm = server ~port:4000 handler in
  match
    Httpcats.request
      ~f:(fun _resp buf str ->
        Buffer.add_string buf str;
        buf)
      ~uri:"http://localhost:4000/" (Buffer.create 0x10)
  with
  | Ok (_response, buf) ->
      Alcotest.(check string)
        "Hello World!" (Buffer.contents buf) "Hello World!";
      Atomic.set stop true;
      Miou.await_exn prm
  | Error err ->
      Atomic.set stop true;
      Miou.await_exn prm;
      Alcotest.failf "Got an error: %a" Httpcats.pp_error err

let () = Alcotest.run "client / server" [ ("simple", [ test00 ]) ]
