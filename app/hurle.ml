let anchor = Unix.gettimeofday ()
let sigpipe = 13
let () = Sys.set_signal sigpipe Sys.Signal_ignore

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
let out = Mutex.create ()

let pr fmt =
  let finally () = Mutex.unlock out in
  Mutex.lock out;
  Fun.protect ~finally @@ fun () -> Format.printf fmt

let epr fmt =
  let finally () = Mutex.unlock out in
  Mutex.lock out;
  Fun.protect ~finally @@ fun () -> Format.eprintf fmt

let getaddrinfo dns =
  {
    Happy.getaddrinfo =
      (fun record host -> Dns_miou.getaddrinfo dns record host);
  }

let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

let () =
  Miou_unix.run @@ fun () ->
  match Sys.argv with
  | [| _; uri |] ->
      let daemon, resolver = Happy.stack () in
      let dns = Dns_miou.create resolver in
      Happy.inject_resolver ~getaddrinfo:(getaddrinfo dns) resolver;
      let acc = Buffer.create 0x100 in
      let f buf str =
        Buffer.add_string buf str;
        buf
      in
      let prm =
        Miou.call @@ fun () ->
        match Httpcats.request ~resolver ~f ~uri acc with
        | Ok (_response, buf) ->
            pr "@[<hov>%a@]\n%!"
              (Hxd_string.pp Hxd.default)
              (Buffer.contents buf)
        | Error err -> epr "Got an error: %a\n%!" Httpcats.pp_error err
      in
      Miou.await_exn prm;
      Happy.kill daemon
  | _ -> epr "%s <uri>\n%!" Sys.argv.(0)
