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
    let open Httpcats.Server in
    let body = "Hello World!" in
    let headers =
      Headers.of_list
        [ ("content-type", "text/plain")
        ; ("content-length", string_of_int (String.length body)) ]
    in
    Httpcats.Server.string ~headers ~status:`OK body
  in
  let stop, prm = server ~port:4000 handler in
  let daemon, resolver = Happy.stack () in
  match
    Httpcats.request ~resolver
      ~f:(fun _resp buf str ->
        Buffer.add_string buf str;
        buf)
      ~uri:"http://127.0.0.1:4000/" (Buffer.create 0x10)
  with
  | Ok (_response, buf) ->
      Alcotest.(check string)
        "Hello World!" (Buffer.contents buf) "Hello World!";
      Atomic.set stop true;
      Miou.await_exn prm;
      Happy.kill daemon
  | Error err ->
      Atomic.set stop true;
      Miou.await_exn prm;
      Happy.kill daemon;
      Alcotest.failf "Got an error: %a" Httpcats.pp_error err

let generate g len =
  let result = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set result i (Char.chr (Random.State.bits g land 0xff))
  done;
  Bytes.unsafe_to_string result

let test01 =
  Alcotest.test_case "stream" `Quick @@ fun () ->
  Miou_unix.run @@ fun () ->
  let g0 = Random.State.make_self_init () in
  let g1 = Random.State.copy g0 in
  let max = 0x100000 in
  let chunk = 0x10 in
  let handler _request =
    let open Httpcats.Server in
    let headers =
      Headers.of_list
        [ ("content-type", "text/plain"); ("content-length", string_of_int max)
        ]
    in
    let stream = Httpcats.Server.stream ~headers `OK in
    let rec go rest =
      if rest <= 0 then stream.close ()
      else
        let len = min chunk rest in
        let str = generate g0 len in
        begin
          stream.write_string str;
          go (rest - len)
        end
    in
    go max
  in
  let stop, prm = server ~port:4000 handler in
  let daemon, resolver = Happy.stack () in
  match
    Httpcats.request ~resolver
      ~f:(fun _resp buf str ->
        Buffer.add_string buf str;
        buf)
      ~uri:"http://127.0.0.1:4000" (Buffer.create 0x1000)
  with
  | Ok (_response, buf) ->
      Alcotest.(check string) "random" (generate g1 max) (Buffer.contents buf);
      Atomic.set stop true;
      Miou.await_exn prm;
      Happy.kill daemon
  | Error err ->
      Atomic.set stop true;
      Miou.await_exn prm;
      Happy.kill daemon;
      Alcotest.failf "Got an error: %a" Httpcats.pp_error err

let () =
  let stdout = Alcotest_engine.Global.make_stdout () in
  let stderr = Alcotest_engine.Global.make_stderr () in
  Alcotest.run ~stdout ~stderr "network" [ ("simple", [ test00; test01 ]) ]
