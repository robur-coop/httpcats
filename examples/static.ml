let error_msgf fmt = Format.kasprintf (fun msg -> Error msg) fmt

let inet_addr_of_string str =
  try Ok (Unix.inet_addr_of_string str)
  with _ -> error_msgf "Invalid address: %S" str

let port_of_string str =
  try Ok (int_of_string str) with _ -> error_msgf "Invalid port: %S" str

let root_of_string str =
  match Fpath.of_string str with
  | Ok root ->
      if Sys.file_exists str && Sys.is_directory str then
        Ok (Fpath.to_dir_path root)
      else error_msgf "%a does not exist" Fpath.pp root
  | Error (`Msg msg) -> Error msg

let default_root = Fpath.(to_dir_path (v (Unix.getcwd ())))

let arguments () =
  match Sys.argv with
  | [| _; address; port; root |] -> begin
      match
        (inet_addr_of_string address, port_of_string port, root_of_string root)
      with
      | Ok inet_addr, Ok port, Ok root ->
          (Unix.ADDR_INET (inet_addr, port), root)
      | Error msg, _, _ | _, Error msg, _ | _, _, Error msg -> failwith msg
    end
  | [| _; address_or_port_or_root |] -> begin
      match
        ( inet_addr_of_string address_or_port_or_root
        , port_of_string address_or_port_or_root
        , root_of_string address_or_port_or_root )
      with
      | Ok inet_addr, _, _ -> (Unix.ADDR_INET (inet_addr, 8080), default_root)
      | _, Ok port, _ ->
          (Unix.ADDR_INET (Unix.inet_addr_loopback, port), default_root)
      | _, _, Ok root -> (Unix.ADDR_INET (Unix.inet_addr_loopback, 8080), root)
      | Error msg, _, _ -> failwith msg
    end
  | [| _ |] -> (Unix.ADDR_INET (Unix.inet_addr_loopback, 8080), default_root)
  | _ ->
      Format.eprintf "%s [<address>] [<port>] [root]\n%!" Sys.executable_name;
      exit 1

let exists ~root path =
  let[@warning "-8"] (Some path) = Fpath.relativize ~root:(Fpath.v "/") path in
  let path = Fpath.(root // path) in
  let path = Fpath.to_string path in
  Sys.file_exists path && Sys.is_directory path = false

let[@warning "-8"] handler ~root _
    (`V1 reqd : [ `V1 of H1.Reqd.t | `V2 of H2.Reqd.t ]) =
  let open H1 in
  let request = Reqd.request reqd in
  match request.Request.target with
  | "" ->
      let text = {text|<h1>Miou, miou, miou!</h1>|text} in
      let headers =
        [
          ("content-type", "text/html; charset=utf-8")
        ; ("content-length", string_of_int (String.length text))
        ]
      in
      let resp = Response.create ~headers:(Headers.of_list headers) `OK in
      let body = Reqd.request_body reqd in
      Body.Reader.close body;
      Reqd.respond_with_string reqd resp text
  | path -> (
      match Fpath.of_string path with
      | Ok path when exists ~root path ->
          let[@warning "-8"] (Some path) =
            Fpath.relativize ~root:(Fpath.v "/") path
          in
          let path = Fpath.(root // path) in
          let ic = open_in Fpath.(to_string (root // path)) in
          let finally () = close_in ic in
          Fun.protect ~finally @@ fun () ->
          let len = in_channel_length ic in
          let headers =
            [
              ("content-type", "text/plain; charset=utf-8")
            ; ("content-length", string_of_int len)
            ]
          in
          let buf = Bytes.create 0x7ff in
          let rec go body =
            match input ic buf 0 (Bytes.length buf) with
            | 0 -> H1.Body.Writer.close body
            | len ->
                let str = Bytes.unsafe_to_string buf in
                H1.Body.Writer.write_string body str ~off:0 ~len;
                Miou.yield ();
                go body
          in
          let headers = Headers.of_list headers in
          let resp = Response.create ~headers `OK in
          let body = H1.Reqd.respond_with_streaming reqd resp in
          go body
      | _ ->
          let headers = Headers.of_list [ ("content-length", "0") ] in
          let resp = Response.create ~headers `Not_found in
          Reqd.respond_with_string reqd resp "")

let server (root, sockaddr) =
  let handler = handler ~root in
  Httpcats.Server.clear ~handler sockaddr

let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let () =
  let addr, root = arguments () in
  let () = Printexc.record_backtrace true in
  Miou_unix.run @@ fun () ->
  let domains = Miou.Domain.available () in
  let prm = Miou.async @@ fun () -> server (root, addr) in
  if domains > 0 then
    Miou.parallel server (List.init domains (Fun.const (root, addr)))
    |> List.iter (function Ok () -> () | Error exn -> raise exn);
  Miou.await_exn prm
