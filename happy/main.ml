let anchor = Mtime_clock.now ()

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      let ts = Mtime.span (Mtime_clock.now ()) anchor in
      let ts = Mtime.Span.to_float_ns ts /. 1e9 in
      Format.kfprintf k ppf
        ("[+%as]%a[%a][%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Magenta (fmt "%04.04f"))
        ts Logs_fmt.pp_header (level, header)
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
let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)
let out = Mutex.create ()

let pr fmt =
  let finally () = Mutex.unlock out in
  Mutex.lock out;
  Fun.protect ~finally @@ fun () -> Format.printf fmt

let epr fmt =
  let finally () = Mutex.unlock out in
  Mutex.lock out;
  Fun.protect ~finally @@ fun () -> Format.eprintf fmt

let pp_msg ppf (`Msg msg) = Fmt.string ppf msg

let getaddrinfo dns =
  {
    Happy.getaddrinfo= (fun record host -> Dns_miou.getaddrinfo dns record host)
  }

let pp_sockaddr ppf (ipaddr, port) =
  Format.fprintf ppf "%a:%d" Ipaddr.pp ipaddr port

let () =
  Miou_unix.run @@ fun () ->
  let daemon, stack = Happy.stack () in
  let dns = Dns_miou.create stack in
  Happy.inject_resolver ~getaddrinfo:(getaddrinfo dns) stack;
  for _ = 0 to 10_000 do
    match Happy.connect_endpoint stack "google.com" [ 443 ] with
    | Ok (sockaddr, fd) ->
        Format.printf "Connected to google.com via %a\n%!" pp_sockaddr sockaddr;
        Miou_unix.close fd
    | Error (`Msg msg) ->
        Logs.err (fun m -> m "Got an error: %s" msg);
        failwith msg
  done;
  Happy.kill daemon
