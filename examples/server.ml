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
let () = Logs.set_level ~all:true (Some Logs.Debug)
let () = Logs_threaded.enable ()
let () = Printexc.record_backtrace true

let text =
  "CHAPTER I. Down the Rabbit-Hole  Alice was beginning to get very tired of \
   sitting by her sister on the bank, and of having nothing to do: once or \
   twice she had peeped into the book her sister was reading, but it had no \
   pictures or conversations in it, <and what is the use of a book,> thought \
   Alice <without pictures or conversations?> So she was considering in her \
   own mind (as well as she could, for the hot day made her feel very sleepy \
   and stupid), whether the pleasure of making a daisy-chain would be worth \
   the trouble of getting up and picking the daisies, when suddenly a White \
   Rabbit with pink eyes ran close by her. There was nothing so very \
   remarkable in that; nor did Alice think it so very much out of the way to \
   hear the Rabbit say to itself, <Oh dear! Oh dear! I shall be late!> (when \
   she thought it over afterwards, it occurred to her that she ought to have \
   wondered at this, but at the time it all seemed quite natural); but when \
   the Rabbit actually took a watch out of its waistcoat-pocket, and looked at \
   it, and then hurried on, Alice started to her feet, for it flashed across \
   her mind that she had never before seen a rabbit with either a \
   waistcoat-pocket, or a watch to take out of it, and burning with curiosity, \
   she ran across the field after it, and fortunately was just in time to see \
   it pop down a large rabbit-hole under the hedge. In another moment down \
   went Alice after it, never once considering how in the world she was to get \
   out again. The rabbit-hole went straight on like a tunnel for some way, and \
   then dipped suddenly down, so suddenly that Alice had not a moment to think \
   about stopping herself before she found herself falling down a very deep \
   well. Either the well was very deep, or she fell very slowly, for she had \
   plenty of time as she went down to look about her and to wonder what was \
   going to happen next. First, she tried to look down and make out what she \
   was coming to, but it was too dark to see anything; then she looked at the \
   sides of the well, and noticed that they were filled with cupboards......"

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

let resp =
  let open H1 in
  let headers =
    Headers.of_list
      [
        ("content-type", "text/plain; charset=utf-8")
      ; ("content-length", string_of_int (String.length text))
      ]
  in
  Response.create ~headers `OK

let sha1 s =
  s
  |> Digestif.SHA1.digest_string
  |> Digestif.SHA1.to_raw_string
  |> Base64.encode_string

let[@warning "-8"] handler _
    (`V1 reqd : [ `V1 of H1.Reqd.t | `V2 of H2.Reqd.t ]) =
  let open H1 in
  let request = Reqd.request reqd in
  match request.Request.target with
  | "" | "/" | "/index.html" ->
      let body = Reqd.request_body reqd in
      Body.Reader.close body;
      Reqd.respond_with_string reqd resp text
  | "/websocket" -> (
      (* TODO only allow a client to upgrade once *)
      match Websocket.Handshake.get_nonce request with
      | None -> failwith "no `sec-websocket-key` header"
      | Some nonce ->
          let hdrs = Websocket.Handshake.server_headers ~sha1 ~nonce in
          Reqd.respond_with_upgrade reqd hdrs)
  | _ ->
      let headers = Headers.of_list [ ("content-length", "0") ] in
      let resp = Response.create ~headers `Not_found in
      Reqd.respond_with_string reqd resp ""

let src = Logs.Src.create "examples/server.ml"

module Log = (val Logs.src_log src : Logs.LOG)

let upgrade flow =
  let fn get put =
    let rec go () = match get () with Some v -> put v; go () | None -> () in
    go ()
  in
  Httpcats.Server.Websocket.upgrade ~fn (`Tcp flow)

let server stop sockaddr =
  Httpcats.Server.clear ~stop ~handler ~upgrade sockaddr

(*let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore*)

let () =
  let addr = sockaddr_of_arguments () in
  let () = Printexc.record_backtrace true in
  Miou_unix.run @@ fun () ->
  let stop = Httpcats.Server.stop () in
  let fn _sigint = Httpcats.Server.switch stop in
  ignore (Miou.sys_signal Sys.sigint (Sys.Signal_handle fn));
  let domains = Miou.Domain.available () in
  let prm = Miou.async @@ fun () -> server stop addr in
  if domains > 0 then
    Miou.parallel (server stop) (List.init domains (Fun.const addr))
    |> List.iter (function Ok () -> () | Error exn -> raise exn);
  Miou.await_exn prm
