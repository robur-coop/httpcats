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

type flow = [ `Tls of Tls_miou_unix.t | `Tcp of Miou_unix.file_descr ]

type request = {
    meth: Method.t
  ; target: string
  ; scheme: string
  ; headers: Headers.t
}

type response = { status: Status.t; headers: Headers.t }
type body = [ `V1 of H1.Body.Writer.t | `V2 of H2.Body.Writer.t ]
type reqd = [ `V1 of H1.Reqd.t | `V2 of H2.Reqd.t ]

type error_handler =
  [ `V1 | `V2 ] -> ?request:request -> error -> (Headers.t -> body) -> unit

type handler =
  [ `Tcp of Miou_unix.file_descr | `Tls of Tls_miou_unix.t ] -> reqd -> unit

let request_from_H1 ~scheme { H1.Request.meth; target; headers; _ } =
  let headers = Headers.of_list (H1.Headers.to_list headers) in
  { meth; target; scheme; headers }

let request_from_h2 { H2.Request.meth; target; scheme; headers } =
  { meth; target; scheme; headers }

let errf = Fmt.str "<h1>500 Internal server error</h1><p>Error: %a</p>" pp_error

let default_error_handler version ?request:_ err respond =
  let str = errf err in
  let hdrs =
    match version with
    | `V1 ->
        [
          ("content-type", "text/html; charset=utf-8")
        ; ("content-length", string_of_int (String.length str))
        ; ("connection", "close")
        ]
    | `V2 ->
        [
          ("content-type", "text/html; charset=utf-8")
        ; ("content-length", string_of_int (String.length str))
        ]
  in
  let hdrs = H2.Headers.of_list hdrs in
  match respond hdrs with
  | `V1 body ->
      H1.Body.Writer.write_string body str;
      let fn () =
        if H1.Body.Writer.is_closed body = false then H1.Body.Writer.close body
      in
      H1.Body.Writer.flush body fn
  | `V2 body ->
      Log.debug (fun m -> m "respond with a h2 error");
      H2.Body.Writer.write_string body str;
      let fn = function
        | `Closed -> ()
        | `Written ->
            Log.debug (fun m -> m "close the h2 body");
            H2.Body.Writer.close body
      in
      Log.debug (fun m -> m "flush the errored h2 response");
      H2.Body.Writer.flush body fn

let http_1_1_server_connection ~config ~user's_error_handler ?upgrade
    ~user's_handler flow =
  let scheme = "http" in
  let read_buffer_size = config.H1.Config.read_buffer_size in
  let error_handler ?request err respond =
    let request = Option.map (request_from_H1 ~scheme) request in
    let err = `V1 err in
    let respond hdrs =
      let hdrs = H1.Headers.of_list (Headers.to_list hdrs) in
      let body = respond hdrs in
      `V1 body
    in
    user's_error_handler `V1 ?request err respond
  in
  let request_handler reqd = user's_handler (`Tcp flow) (`V1 reqd) in
  let conn =
    H1.Server_connection.create ~config ~error_handler request_handler
  in
  (* NOTE(dinosaure): see the module [TCP_and_H1] and the fake shutdown. We must
     finalize the process with [Miou_unix.close flow] — and avoid a fd leak. At
     the end, the flow is only shutdown on the write side. *)
  let finally () = Miou_unix.close flow in
  Fun.protect ~finally @@ fun () ->
  Miou.await_exn (B.run conn ~read_buffer_size ?upgrade flow)

let https_1_1_server_connection ~config ~user's_error_handler ?upgrade
    ~user's_handler flow =
  let scheme = "https" in
  let read_buffer_size = config.H1.Config.read_buffer_size in
  let error_handler ?request err respond =
    let request = Option.map (request_from_H1 ~scheme) request in
    let err = `V1 err in
    let respond hdrs =
      let hdrs = H1.Headers.of_list (Headers.to_list hdrs) in
      let body = respond hdrs in
      `V1 body
    in
    user's_error_handler `V1 ?request err respond
  in
  let request_handler reqd = user's_handler (`Tls flow) (`V1 reqd) in
  let conn =
    H1.Server_connection.create ~config ~error_handler request_handler
  in
  Miou.await_exn (A.run conn ~read_buffer_size ?upgrade flow)

let h2s_server_connection ~config ~user's_error_handler ?upgrade ~user's_handler
    flow =
  let read_buffer_size = config.H2.Config.read_buffer_size in
  let error_handler ?request err respond =
    let request = Option.map request_from_h2 request in
    let err = `V2 err in
    let respond hdrs = `V2 (respond hdrs) in
    user's_error_handler `V2 ?request err respond
  in
  let request_handler reqd = user's_handler (`Tls flow) (`V2 reqd) in
  let conn =
    H2.Server_connection.create ~config ~error_handler request_handler
  in
  Miou.await_exn (C.run conn ~read_buffer_size ?upgrade flow)

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
      let stop = Miou.async (wait stop) in
      Log.debug (fun m -> m "waiting for a client");
      match Miou.await_first [ accept; stop ] with
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

let clear ?(parallel = true) ?stop ?(config = H1.Config.default) ?backlog ?ready
    ?error_handler:(user's_error_handler = default_error_handler) ?upgrade
    ~handler:user's_handler sockaddr =
  let domains = Miou.Domain.available () in
  let call ~orphans fn =
    if parallel && domains >= 2 then ignore (Miou.call ~orphans fn)
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
              http_1_1_server_connection ~config ~user's_error_handler ?upgrade
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
  Option.iter (fun c -> ignore (Miou.Computation.try_return c ())) ready;
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

let with_tls ?(parallel = true) ?stop
    ?(config = `Both (H1.Config.default, H2.Config.default)) ?backlog ?ready
    ?error_handler:(user's_error_handler = default_error_handler) tls_config
    ?upgrade ~handler:user's_handler sockaddr =
  let domains = Miou.Domain.available () in
  let call ~orphans fn =
    if parallel && domains >= 2 then ignore (Miou.call ~orphans fn)
    else ignore (Miou.async ~orphans fn)
  in
  let rec go orphans file_descr =
    match accept_or_stop ?stop file_descr with
    | None -> Runtime.terminate orphans; Miou_unix.close file_descr
    | Some (fd', _sockaddr) ->
        clean_up orphans;
        let fn () =
          try
            let tls_flow = Tls_miou_unix.server_of_fd tls_config fd' in
            begin
              match (config, alpn tls_flow) with
              | `Both (_, h2), Some "h2" | `H2 h2, (Some "h2" | None) ->
                  Log.debug (fun m -> m "Start a h2 request handler");
                  h2s_server_connection ~config:h2 ~user's_error_handler
                    ?upgrade ~user's_handler tls_flow
              | `Both (config, _), Some "http/1.1"
              | `HTTP_1_1 config, (Some "http/1.1" | None) ->
                  Log.debug (fun m -> m "Start a http/1.1 request handler");
                  https_1_1_server_connection ~config ~user's_error_handler
                    ?upgrade ~user's_handler tls_flow
              | `Both _, None -> assert false
              | _, Some _protocol -> assert false
            end
          with exn ->
            Log.err (fun m ->
                m "got a TLS error during the handshake: %s"
                  (Printexc.to_string exn));
            Miou_unix.close fd'
        in
        call ~orphans fn; go orphans file_descr
  in
  let socket =
    match sockaddr with
    | Unix.ADDR_UNIX _ -> invalid_arg "Impossible to create a Unix socket"
    | Unix.ADDR_INET (inet_addr, _) ->
        if Unix.is_inet6_addr inet_addr then Miou_unix.tcpv6 ()
        else Miou_unix.tcpv4 ()
  in
  Miou_unix.bind_and_listen ?backlog socket sockaddr;
  Option.iter (fun c -> ignore (Miou.Computation.try_return c ())) ready;
  go (Miou.orphans ()) socket

module Websocket_connection = struct
  (* make it match Runtime.S signature *)
  include H1_ws.Server_connection

  let next_read_operation t =
    (next_read_operation t :> [ `Read | `Close | `Upgrade | `Yield ])

  let next_write_operation t =
    (next_write_operation t
      :> [ `Write of Bigstringaf.t Faraday.iovec list
         | `Close of int
         | `Yield
         | `Upgrade ])

  let yield_reader _t _k = assert false

  let report_exn _t exn =
    Log.err (fun m -> m "websocket runtime: report_exn");
    raise exn
end

module D = Runtime.Make (TCP_and_H1) (Websocket_connection)
module E = Runtime.Make (Tls_miou_unix) (Websocket_connection)
module Bstream = Bstream

type elt =
  ([ `Connection_close
   | `Msg of H1_ws.Websocket.Opcode.standard_non_control * bool
   | `Other
   | `Ping
   | `Pong ]
  * bytes)
  Bstream.t

open H1_ws

module Close_state = struct
  (* TODO do we need the lock? *)
  type t = {
      lock: Miou.Mutex.t
    ; cond: Miou.Condition.t
    ; mutable received: bool
    ; mutable emitted: bool
    ; mutable eof: bool
  }

  let create () =
    {
      lock= Miou.Mutex.create ()
    ; cond= Miou.Condition.create ()
    ; received= false
    ; emitted= false
    ; eof= false
    }

  let set_received t =
    Miou.Mutex.protect t.lock @@ fun () ->
    t.received <- true;
    Miou.Condition.signal t.cond

  let set_emmited t =
    Miou.Mutex.protect t.lock @@ fun () ->
    t.emitted <- true;
    Miou.Condition.signal t.cond

  let set_eof t =
    Miou.Mutex.protect t.lock @@ fun () ->
    t.eof <- true;
    Miou.Condition.signal t.cond

  let on_close t f =
    Miou.Mutex.protect t.lock @@ fun () ->
    while not ((t.received && t.emitted) || t.eof) do
      Miou.Condition.wait t.cond t.lock
    done;
    f t.received t.emitted
end

let write_websocket oc close_state wsd =
  let rec go () =
    match Bstream.get oc with
    | None -> ()
    | Some (kind, data) ->
        begin
          match kind with
          | `Other -> failwith "Unsupported frame of kind `Other"
          | `Connection_close ->
              Wsd.close wsd;
              Bstream.close oc;
              Close_state.set_emmited close_state;
              ()
          | `Ping -> Wsd.send_ping wsd
          | `Pong -> Wsd.send_pong wsd
          | `Msg (kind, is_fin) ->
              let len = Bytes.length data in
              Wsd.send_bytes wsd ~kind ~is_fin data ~off:0 ~len;
              ()
        end;
        go ()
  in
  go

let websocket_handler ic ivar close_state wsd =
  ignore (Miou.Computation.try_return ivar wsd);
  let frame_handler ~opcode ~is_fin bstr ~off ~len =
    let data =
      let s = Bigstringaf.substring bstr ~off ~len in
      String.to_bytes s
    in
    match opcode with
    | `Other _ -> Bstream.put ic (`Other, data)
    | `Connection_close ->
        Bstream.put ic (`Connection_close, data);
        Close_state.set_received close_state;
        Bstream.close ic
    | #Websocket.Opcode.standard_control as kind -> Bstream.put ic (kind, data)
    | #Websocket.Opcode.standard_non_control as kind ->
        Bstream.put ic (`Msg (kind, is_fin), data)
  in
  let eof () =
    Close_state.set_eof close_state;
    ()
  in
  Websocket.{ frame_handler; eof }

let websocket_upgrade ~fn flow =
  let ic = Bstream.create 0x100 in
  let oc = Bstream.create 0x100 in
  let ivar = Miou.Computation.create () in
  let close_state = Close_state.create () in
  let websocket_handler = websocket_handler ic ivar close_state in
  let conn =
    H1_ws.Server_connection.create ~websocket_handler
    (* wsd -> input_handlers *)
  in
  let runtime's_prm =
    match flow with
    | `Tcp flow -> D.run conn flow
    | `Tls flow -> E.run conn flow
  in
  let wsd = Miou.Computation.await_exn ivar in
  let writer = Miou.async (write_websocket oc close_state wsd) in
  let user's_handler = Miou.async @@ fun () -> fn ic oc in
  let close =
    Miou.async @@ fun () ->
    Close_state.on_close close_state @@ fun received emitted ->
    if received && emitted then (
      Log.debug (fun m -> m "websocket clean close");
      (* ic and oc have already been closed *)
      ())
    else (
      Log.debug (fun m -> m "websocket unclean close");
      (* we need to close the runtime's writer here
               [Wsd.close] close the writer and also write a close frame *)
      Wsd.close wsd;
      (* ic and oc may not have been closed
               we halt instead of close to disregard any data left on the streams *)
      Bstream.halt ic;
      Bstream.halt oc;
      ())
  in
  Miou.await_all [ runtime's_prm; user's_handler; writer; close ]
  |> List.iter (function Ok () -> () | Error exn -> raise exn)
