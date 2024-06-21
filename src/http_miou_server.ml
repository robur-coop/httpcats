open Http_miou_unix
module A = Runtime.Make (Tls_miou_unix) (Httpaf.Server_connection)

module TCP_and_httpaf = struct
  include TCP

  let shutdown flow = function `read -> () | value -> shutdown flow value
end

module B = Runtime.Make (TCP_and_httpaf) (Httpaf.Server_connection)
module C = Runtime.Make (Tls_miou_unix) (H2.Server_connection)

type error =
  [ `V1 of Httpaf.Server_connection.error
  | `V2 of H2.Server_connection.error
  | `Protocol of string ]

type stop = Miou.Mutex.t * Miou.Condition.t * bool ref

let pp_error ppf = function
  | `V1 `Bad_request -> Fmt.string ppf "Bad HTTP/1.1 request"
  | `V1 `Bad_gateway -> Fmt.string ppf "Bad HTTP/1.1 gateway"
  | `V1 `Internal_server_error | `V2 `Internal_server_error ->
      Fmt.string ppf "Internal server error"
  | `V1 (`Exn exn) | `V2 (`Exn exn) ->
      Fmt.pf ppf "Got an unexpected exception: %S" (Printexc.to_string exn)
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
type body = [ `V1 of [ `write ] Httpaf.Body.t | `V2 of H2.Body.Writer.t ]
type reqd = [ `V1 of Httpaf.Reqd.t | `V2 of H2.Reqd.t ]
type error_handler = ?request:request -> error -> (Headers.t -> body) -> unit
type handler = reqd -> unit

let request_from_httpaf ~scheme { Httpaf.Request.meth; target; headers; _ } =
  let headers = Headers.of_list (Httpaf.Headers.to_list headers) in
  { meth; target; scheme; headers }

let request_from_h2 { H2.Request.meth; target; scheme; headers } =
  { meth; target; scheme; headers }

let default_error_handler ?request:_ _err _respond = ()

let http_1_1_server_connection ?(config = Httpaf.Config.default)
    ?(error_handler = default_error_handler) ~handler flow =
  let scheme = "http" in
  let read_buffer_size = config.Httpaf.Config.read_buffer_size in
  let error_handler ?request err respond =
    let request = Option.map (request_from_httpaf ~scheme) request in
    let err =
      match err with `Exn (Runtime.Flow msg) -> `Protocol msg | err -> `V1 err
    in
    Runtime.flat_tasks @@ fun orphans ->
    let respond hdrs =
      let hdrs = Httpaf.Headers.of_list (Headers.to_list hdrs) in
      let body = respond hdrs in
      `V1 body
    in
    error_handler ?request err respond;
    Runtime.terminate orphans
  in
  let request_handler reqd =
    Runtime.flat_tasks @@ fun orphans -> handler reqd; Runtime.terminate orphans
  in
  let conn =
    Httpaf.Server_connection.create ~config ~error_handler request_handler
  in
  Miou.await_exn (B.run conn ~read_buffer_size flow)

let https_1_1_server_connection ?(config = Httpaf.Config.default)
    ?(error_handler = default_error_handler) ~handler flow =
  let scheme = "https" in
  let read_buffer_size = config.Httpaf.Config.read_buffer_size in
  let error_handler ?request err respond =
    let request = Option.map (request_from_httpaf ~scheme) request in
    let err =
      match err with `Exn (Runtime.Flow msg) -> `Protocol msg | err -> `V1 err
    in
    Runtime.flat_tasks @@ fun orphans ->
    let respond hdrs =
      let hdrs = Httpaf.Headers.of_list (Headers.to_list hdrs) in
      let body = respond hdrs in
      `V1 body
    in
    error_handler ?request err respond;
    Runtime.terminate orphans
  in
  let request_handler reqd =
    Runtime.flat_tasks @@ fun orphans -> handler reqd; Runtime.terminate orphans
  in
  let conn =
    Httpaf.Server_connection.create ~config ~error_handler request_handler
  in
  Miou.await_exn (A.run conn ~read_buffer_size flow)

let h2s_server_connection ?(config = H2.Config.default)
    ?(error_handler = default_error_handler) ~handler flow =
  let read_buffer_size = config.H2.Config.read_buffer_size in
  let error_handler ?request err respond =
    let request = Option.map request_from_h2 request in
    let err =
      match err with `Exn (Runtime.Flow msg) -> `Protocol msg | err -> `V2 err
    in
    Runtime.flat_tasks @@ fun orphans ->
    let respond hdrs = `V2 (respond hdrs) in
    error_handler ?request err respond;
    Runtime.terminate orphans
  in
  let request_handler reqd =
    Runtime.flat_tasks @@ fun orphans -> handler reqd; Runtime.terminate orphans
  in
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
      let accept = Miou.call_cc @@ fun () -> Miou_unix.accept file_descr in
      let wait = Miou.call_cc (wait stop) in
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

let clear ?stop ?config ?backlog ?error_handler ~handler sockaddr =
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
        let _ =
          Miou.call ~orphans @@ fun () ->
          http_1_1_server_connection ?config ?error_handler ~handler fd'
        in
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

let with_tls ?stop ?(config = `Both (Httpaf.Config.default, H2.Config.default))
    ?backlog ?error_handler tls_config ~handler:(user's_handler : handler)
    sockaddr =
  let rec go orphans file_descr =
    match accept_or_stop ?stop file_descr with
    | None -> Runtime.terminate orphans; Miou_unix.close file_descr
    | Some (fd', _sockaddr) ->
        clean_up orphans;
        let _ =
          Miou.call ~orphans @@ fun () ->
          try
            let tls_flow = Tls_miou_unix.server_of_fd tls_config fd' in
            begin
              match (config, alpn tls_flow) with
              | `Both (_, h2), Some "h2" | `H2 h2, (Some "h2" | None) ->
                  let handler reqd = user's_handler (`V2 reqd) in
                  h2s_server_connection ~config:h2 ?error_handler ~handler
                    tls_flow
              | `Both (httpaf, _), Some "http/1.1"
              | `HTTP_1_1 httpaf, (Some "http/1.1" | None) ->
                  let handler reqd = user's_handler (`V1 reqd) in
                  https_1_1_server_connection ~config:httpaf ?error_handler
                    ~handler tls_flow
              | `Both _, None -> assert false
              | _, Some _protocol -> assert false
            end
          with exn ->
            Log.err (fun m ->
                m "got a TLS error during the handshake: %s"
                  (Printexc.to_string exn));
            Miou_unix.close fd'
        in
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
