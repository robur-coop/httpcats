open Http_miou_unix
module A = Runtime.Make (Tls_miou_unix) (H1.Server_connection)

module TCP_and_H1 = struct
  include TCP

  (* NOTE(dinosaure): an early [shutdown `read] is not really appreciated by
     http/1.1 servers. We do nothing in this case. However, we do make sure that
     as soon as the process has finished, we close the socket.

     See [http_1_1_server_connection] for the [Unix.close]. *)
  let shutdown flow = function `read -> () | value -> shutdown flow value
end

module H2_Server_connection = struct
  include H2.Server_connection

  let next_read_operation t =
    (next_read_operation t :> [ `Close | `Read | `Yield | `Upgrade ])

  let next_write_operation t =
    (next_write_operation t
      :> [ `Close of int
         | `Write of Bigstringaf.t Faraday.iovec list
         | `Yield
         | `Upgrade ])
end

module B = Runtime.Make (TCP_and_H1) (H1.Server_connection)
module C = Runtime.Make (TLS) (H2_Server_connection)

type error =
  [ `V1 of H1.Server_connection.error
  | `V2 of H2.Server_connection.error
  | `Protocol of string ]

type stop = Miou.Mutex.t * Miou.Condition.t * bool ref

let pp_error ppf = function
  | `V1 `Bad_request -> Fmt.string ppf "Bad HTTP/1.1 request"
  | `V1 `Bad_gateway -> Fmt.string ppf "Bad HTTP/1.1 gateway"
  | `V1 `Internal_server_error | `V2 `Internal_server_error ->
      Fmt.string ppf "Internal server error"
  | `V1 (`Exn exn) | `V2 (`Exn exn) ->
      Fmt.pf ppf "Unknown exception: %s" (Printexc.to_string exn)
  | `V2 `Bad_request -> Fmt.string ppf "Bad H2 request"
  | `Protocol msg -> Fmt.string ppf msg

let src = Logs.Src.create "http-miou-server"

module Log = (val Logs.src_log src : Logs.LOG)
module Method = H2.Method
module Headers = H2.Headers
module Status = H2.Status

type request = {
    meth: Method.t
  ; target: string
  ; scheme: string
  ; headers: Headers.t
}

type response = { status: Status.t; headers: Headers.t }
type body = [ `V1 of H1.Body.Writer.t | `V2 of H2.Body.Writer.t ]
type reqd = [ `V1 of H1.Reqd.t | `V2 of H2.Reqd.t ]
type error_handler = ?request:request -> error -> (Headers.t -> body) -> unit

type handler =
  [ `Tcp of Miou_unix.file_descr | `Tls of Tls_miou_unix.t ] -> reqd -> unit

let request_from_H1 ~scheme { H1.Request.meth; target; headers; _ } =
  let headers = Headers.of_list (H1.Headers.to_list headers) in
  { meth; target; scheme; headers }

let request_from_h2 { H2.Request.meth; target; scheme; headers } =
  { meth; target; scheme; headers }

let default_error_handler ?request:_ err respond =
  let str =
    Fmt.str {html|<h1>500 Internal server error</h1><p>Error: %a</p>|html}
      pp_error err
  in
  let hdrs =
    [
      ("content-type", "text/html; charset=utf-8")
    ; ("content-length", string_of_int (String.length str))
    ; ("connection", "close")
    ]
  in
  let hdrs = Headers.of_list hdrs in
  match respond hdrs with
  | `V1 body ->
      H1.Body.Writer.write_string body str;
      H1.Body.Writer.close body
  | `V2 body ->
      H2.Body.Writer.write_string body str;
      H2.Body.Writer.close body

let http_1_1_server_connection ~config ~user's_error_handler ~user's_handler
    flow =
  let scheme = "http" in
  let read_buffer_size = config.H1.Config.read_buffer_size in
  let error_handler ?request err respond =
    let request = Option.map (request_from_H1 ~scheme) request in
    let err =
      match err with `Exn (Runtime.Flow msg) -> `Protocol msg | err -> `V1 err
    in
    let respond hdrs =
      let hdrs = H1.Headers.of_list (Headers.to_list hdrs) in
      let body = respond hdrs in
      `V1 body
    in
    user's_error_handler ?request err respond
  in
  let request_handler reqd = user's_handler (`Tcp flow) (`V1 reqd) in
  let conn =
    H1.Server_connection.create ~config ~error_handler request_handler
  in
  (* NOTE(dinosaure): see the module [TCP_and_H1] and the fake shutdown. We must
     finalize the process with [Miou_unix.close flow]. At the end, the flow is
     only shutdown on the write side. *)
  let finally () = Miou_unix.close flow in
  Fun.protect ~finally @@ fun () ->
  Miou.await_exn (B.run conn ~read_buffer_size flow)

let https_1_1_server_connection ~config ~user's_error_handler ~user's_handler
    flow =
  let scheme = "https" in
  let read_buffer_size = config.H1.Config.read_buffer_size in
  let error_handler ?request err respond =
    let request = Option.map (request_from_H1 ~scheme) request in
    let err =
      match err with `Exn (Runtime.Flow msg) -> `Protocol msg | err -> `V1 err
    in
    let respond hdrs =
      let hdrs = H1.Headers.of_list (Headers.to_list hdrs) in
      let body = respond hdrs in
      `V1 body
    in
    user's_error_handler ?request err respond
  in
  let request_handler reqd = user's_handler (`Tls flow) (`V1 reqd) in
  let conn =
    H1.Server_connection.create ~config ~error_handler request_handler
  in
  Miou.await_exn (A.run conn ~read_buffer_size flow)

let h2s_server_connection ~config ~user's_error_handler ~user's_handler flow =
  let read_buffer_size = config.H2.Config.read_buffer_size in
  let error_handler ?request err respond =
    let request = Option.map request_from_h2 request in
    let err =
      match err with `Exn (Runtime.Flow msg) -> `Protocol msg | err -> `V2 err
    in
    let respond hdrs = `V2 (respond hdrs) in
    user's_error_handler ?request err respond
  in
  let request_handler reqd = user's_handler (`Tls flow) (`V2 reqd) in
  let conn =
    H2.Server_connection.create ~config ~error_handler request_handler
  in
  Miou.await_exn (C.run conn ~read_buffer_size flow)

let rec clean_up orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) -> (
      match Miou.await prm with
      | Ok () -> clean_up orphans
      | Error exn ->
          Log.err (fun m ->
              m "unexpected exception: %s" (Printexc.to_string exn));
          clean_up orphans)

exception Stop

let rec wait ((m, c, v) as stop) () =
  let value =
    Miou.Mutex.protect m @@ fun () ->
    while not !v do
      Miou.Condition.wait c m
    done;
    !v
  in
  if value then raise Stop else wait stop ()

let stop () = (Miou.Mutex.create (), Miou.Condition.create (), ref false)

let switch (m, c, v) =
  Miou.Mutex.protect m @@ fun () ->
  v := true;
  Miou.Condition.broadcast c

let accept_or_stop ?stop file_descr =
  match stop with
  | None -> Some (Miou_unix.accept file_descr)
  | Some stop -> (
      let accept = Miou.async @@ fun () -> Miou_unix.accept file_descr in
      let wait = Miou.async (wait stop) in
      Log.debug (fun m -> m "waiting for a client");
      match Miou.await_first [ accept; wait ] with
      | Ok (fd, sockaddr) -> Some (fd, sockaddr)
      | Error Stop -> None
      | Error exn ->
          Log.err (fun m ->
              m "unexpected exception: %S" (Printexc.to_string exn));
          raise exn)

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX str -> Fmt.pf ppf "<%s>" str
  | Unix.ADDR_INET (inet_addr, port) ->
      Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port

let clear ?stop ?(config = H1.Config.default) ?backlog
    ?error_handler:(user's_error_handler = default_error_handler)
    ~handler:user's_handler sockaddr =
  let domains = Miou.Domain.available () in
  let call ~orphans fn =
    if domains >= 2 then ignore (Miou.call ~orphans fn)
    else ignore (Miou.async ~orphans fn)
  in
  let rec go orphans file_descr =
    match accept_or_stop ?stop file_descr with
    | None ->
        Log.debug (fun m -> m "stop the server");
        Runtime.terminate orphans;
        Miou_unix.close file_descr
    | Some (fd', sockaddr) ->
        Log.debug (fun m ->
            m "receive a connection from: %a" pp_sockaddr sockaddr);
        clean_up orphans;
        call ~orphans
          begin
            fun () ->
              http_1_1_server_connection ~config ~user's_error_handler
                ~user's_handler fd'
          end;
        go orphans file_descr
  in
  let socket =
    match sockaddr with
    | Unix.ADDR_UNIX _ -> invalid_arg "Impossible to create a Unix socket"
    | Unix.ADDR_INET (inet_addr, _) ->
        if Unix.is_inet6_addr inet_addr then Miou_unix.tcpv6 ()
        else Miou_unix.tcpv4 ()
  in
  Miou_unix.bind_and_listen ?backlog socket sockaddr;
  go (Miou.orphans ()) socket

let alpn tls =
  match Tls_miou_unix.epoch tls with
  | Some { Tls.Core.alpn_protocol= protocol; _ } ->
      Log.debug (fun m ->
          m "protocol of the incoming client: %a"
            Fmt.(Dump.option string)
            protocol);
      protocol
  | None -> None

let with_tls ?stop ?(config = `Both (H1.Config.default, H2.Config.default))
    ?backlog ?error_handler:(user's_error_handler = default_error_handler)
    tls_config ~handler:user's_handler sockaddr =
  let domains = Miou.Domain.available () in
  let call ~orphans fn =
    if domains >= 2 then ignore (Miou.call ~orphans fn)
    else ignore (Miou.async ~orphans fn)
  in
  let rec go orphans file_descr =
    match accept_or_stop ?stop file_descr with
    | None -> Runtime.terminate orphans; Miou_unix.close file_descr
    | Some (fd', _sockaddr) ->
        clean_up orphans;
        call ~orphans
          begin
            fun () ->
              try
                let tls_flow = Tls_miou_unix.server_of_fd tls_config fd' in
                begin
                  match (config, alpn tls_flow) with
                  | `Both (_, h2), Some "h2" | `H2 h2, (Some "h2" | None) ->
                      Log.debug (fun m -> m "Start a h2 request handler");
                      h2s_server_connection ~config:h2 ~user's_error_handler
                        ~user's_handler tls_flow
                  | `Both (config, _), Some "http/1.1"
                  | `HTTP_1_1 config, (Some "http/1.1" | None) ->
                      Log.debug (fun m -> m "Start a http/1.1 request handler");
                      https_1_1_server_connection ~config ~user's_error_handler
                        ~user's_handler tls_flow
                  | `Both _, None -> assert false
                  | _, Some _protocol -> assert false
                end
              with exn ->
                Log.err (fun m ->
                    m "got a TLS error during the handshake: %s"
                      (Printexc.to_string exn));
                Miou_unix.close fd'
          end;
        go orphans file_descr
  in
  let socket =
    match sockaddr with
    | Unix.ADDR_UNIX _ -> invalid_arg "Impossible to create a Unix socket"
    | Unix.ADDR_INET (inet_addr, _) ->
        if Unix.is_inet6_addr inet_addr then Miou_unix.tcpv6 ()
        else Miou_unix.tcpv4 ()
  in
  Miou_unix.bind_and_listen ?backlog socket sockaddr;
  go (Miou.orphans ()) socket
