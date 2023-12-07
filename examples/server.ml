open Httpcats.Server

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

let index_html =
  {html|<html>
  <head>
    <title>httpcats</title>
  </head>
  <body>
    <h1>Hello World!</h1>
  </body>
</html>|html}

let error_msgf fmt = Format.kasprintf (fun msg -> Error msg) fmt

let inet_addr_of_string str =
  try Ok (Unix.inet_addr_of_string str)
  with _ -> error_msgf "Invalid address: %S" str

let port_of_string str =
  try Ok (int_of_string str) with _ -> error_msgf "Invalid port: %S" str

let sockaddr_of_arguments () =
  match Sys.argv with
  | [| _; address; port |] -> begin
      match (inet_addr_of_string address, port_of_string port) with
      | Ok inet_addr, Ok port -> Unix.ADDR_INET (inet_addr, port)
      | Error msg, _ | _, Error msg -> failwith msg
    end
  | [| _; address_or_port |] -> begin
      match
        (inet_addr_of_string address_or_port, port_of_string address_or_port)
      with
      | Ok inet_addr, _ -> Unix.ADDR_INET (inet_addr, 8080)
      | _, Ok port -> Unix.ADDR_INET (Unix.inet_addr_loopback, port)
      | Error msg, _ -> failwith msg
    end
  | [| _ |] -> Unix.ADDR_INET (Unix.inet_addr_loopback, 8080)
  | _ ->
      Format.eprintf "%s [<address>] [<port>]\n%!" Sys.executable_name;
      exit 1

let listen sockaddr =
  let file_descr =
    match sockaddr with
    | Unix.ADDR_INET (inet_addr, _) ->
        if Unix.is_inet6_addr inet_addr then Miou_unix.tcpv6 ()
        else Miou_unix.tcpv4 ()
    | _ -> failwith "Invalid address"
  in
  Miou_unix.bind_and_listen file_descr sockaddr;
  file_descr

let rec cleanup orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) ->
      Miou.await_exn prm;
      cleanup orphans

let handler request =
  match request.target with
  | "" | "/" | "/index.html" ->
      let headers =
        Headers.of_list
          [ ("content-type", "text/html; charset=utf-8")
          ; ("content-length", string_of_int (String.length index_html)) ]
      in
      string ~headers ~status:`OK index_html
  | _ ->
      let headers = Headers.of_list [ ("content-length", "0") ] in
      string ~headers ~status:`Not_found ""

let stop = Miou_unix.Cond.make ()

let server sockaddr =
  let file_descr = listen sockaddr in
  Httpcats.Server.clear ~stop ~handler file_descr;
  Miou_unix.disown file_descr

let stop _ = Miou_unix.Cond.broadcast stop

let () =
  let addr = sockaddr_of_arguments () in
  let () = Sys.set_signal Sys.sigint (Signal_handle stop) in
  let () = Printexc.record_backtrace true in
  Miou_unix.run @@ fun () ->
  let prm = Miou.call_cc @@ fun () -> server addr in
  Miou.parallel server (List.init (Miou.Domain.count ()) (Fun.const addr))
  |> List.iter (function Ok () -> () | Error exn -> raise exn);
  Miou.await_exn prm
