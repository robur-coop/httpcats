open Http_miou_unix

let peer = Logs.Tag.def ~doc:"HTTPcats peer" "httpcats.peer" Fmt.string

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
         | `Write of Bstr.t Faraday.iovec list
         | `Yield
         | `Upgrade ])
end

module A = Runtime.Make (Tls_miou_unix) (H1.Server_connection)
module B = Runtime.Make (TCP_and_H1) (H1.Server_connection)
module C = Runtime.Make (TLS) (H2_Server_connection)

type error = Httpcats_core.Server.error

type stop = {
    mutex: Miou.Mutex.t
  ; condition: Miou.Condition.t
  ; flag: bool Atomic.t
}

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

type request = Httpcats_core.Server.request = {
    meth: Method.t
  ; target: string
  ; scheme: string
  ; headers: Headers.t
}

type response = Httpcats_core.Server.response = {
    status: Status.t
  ; headers: Headers.t
}

type body = [ `V1 of H1.Body.Writer.t | `V2 of H2.Body.Writer.t ]
type reqd = [ `V1 of H1.Reqd.t | `V2 of H2.Reqd.t ]

type listen =
  | Bind of Unix.sockaddr
  | Use of Miou_unix.file_descr * Unix.sockaddr

type error_handler = Httpcats_core.Server.error_handler

type handler =
  [ `Tcp of Miou_unix.file_descr | `Tls of Tls_miou_unix.t ]
  Httpcats_core.Server.handler

let request_from_h1 ~scheme { H1.Request.meth; target; headers; _ } =
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

let pp_sockaddr ppf = function
  | Unix.ADDR_INET (inet_addr, port) ->
      Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port
  | Unix.ADDR_UNIX str -> Fmt.pf ppf "<%s>" str

let http_1_1_server_connection ~config ~user's_error_handler ?upgrade
    ~user's_handler flow =
  let scheme = "http" in
  let read_buffer_size = config.H1.Config.read_buffer_size in
  let error_handler ?request err respond =
    let request = Option.map (request_from_h1 ~scheme) request in
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
  let finally flow = try Miou_unix.close flow with _exn -> () in
  let res = Miou.Ownership.create ~finally flow in
  Miou.Ownership.own res;
  let tags =
    match Logs.Src.level src with
    | Some Logs.Debug ->
        let sockaddr = Unix.getpeername (Miou_unix.to_file_descr flow) in
        let str = Fmt.str "http://%a" pp_sockaddr sockaddr in
        Logs.Tag.add peer str Logs.Tag.empty
    | _ -> Logs.Tag.empty
  in
  Miou.await_exn (B.run conn ~tags ~read_buffer_size ?upgrade flow);
  Miou.Ownership.release res

let https_1_1_server_connection ~config ~user's_error_handler ?upgrade
    ~user's_handler flow =
  let scheme = "https" in
  let read_buffer_size = config.H1.Config.read_buffer_size in
  let error_handler ?request err respond =
    let request = Option.map (request_from_h1 ~scheme) request in
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
  let finally flow = try Tls_miou_unix.close flow with _exn -> () in
  let res = Miou.Ownership.create ~finally flow in
  Miou.Ownership.own res;
  let tags =
    match Logs.Src.level src with
    | Some Logs.Debug ->
        let fd = Tls_miou_unix.file_descr flow in
        let fd = Miou_unix.to_file_descr fd in
        let sockaddr = Unix.getpeername fd in
        let str = Fmt.str "https://%a" pp_sockaddr sockaddr in
        Logs.Tag.add peer str Logs.Tag.empty
    | _ -> Logs.Tag.empty
  in
  Miou.await_exn (A.run conn ~tags ~read_buffer_size ?upgrade flow);
  Miou.Ownership.release res

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
  let finally flow = try Tls_miou_unix.close flow with _exn -> () in
  let res = Miou.Ownership.create ~finally flow in
  Miou.Ownership.own res;
  let tags =
    match Logs.Src.level src with
    | Some Logs.Debug ->
        let fd = Tls_miou_unix.file_descr flow in
        let fd = Miou_unix.to_file_descr fd in
        let sockaddr = Unix.getpeername fd in
        let str = Fmt.str "https://%a" pp_sockaddr sockaddr in
        Logs.Tag.add peer str Logs.Tag.empty
    | _ -> Logs.Tag.empty
  in
  let conn =
    H2.Server_connection.create ~config ~error_handler request_handler
  in
  Miou.await_exn (C.run conn ~tags ~read_buffer_size ?upgrade flow);
  Miou.Ownership.release res

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

let rec wait ({ mutex; condition; flag } as stop) () =
  let value =
    Miou.Mutex.protect mutex @@ fun () ->
    while Atomic.get flag = false do
      Miou.Condition.wait condition mutex
    done;
    Atomic.get flag
  in
  if value then raise Stop else wait stop ()

let stop () =
  let mutex = Miou.Mutex.create () in
  let condition = Miou.Condition.create () in
  let flag = Atomic.make false in
  { mutex; condition; flag }

let switch { mutex; condition; flag } =
  Miou.Mutex.protect mutex @@ fun () ->
  Atomic.set flag true;
  Miou.Condition.broadcast condition

let accept_or_stop ?stop file_descr =
  match stop with
  | None -> Some (Miou_unix.accept file_descr)
  | Some s -> begin
      if Atomic.get s.flag then None
      else
        let accept = Miou.async @@ fun () -> Miou_unix.accept file_descr in
        let stop = Miou.async (wait s) in
        Log.debug (fun m -> m "waiting for a client");
        match Miou.await_first [ accept; stop ] with
        | Ok (fd, _sockaddr) when Atomic.get s.flag -> Miou_unix.close fd; None
        | Ok (fd, sockaddr) -> Some (fd, sockaddr)
        | Error Stop -> None
        | Error _exn when Atomic.get s.flag -> None
        | Error exn ->
            Log.err (fun m ->
                m "unexpected exception: %S" (Printexc.to_string exn));
            raise exn
    end

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX str -> Fmt.pf ppf "<%s>" str
  | Unix.ADDR_INET (inet_addr, port) ->
      Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port

let inhibit fn = try fn () with _exn -> ()

let listen_to_fd backlog = function
  | Use (fd, sockaddr) -> (fd, sockaddr)
  | Bind sockaddr ->
      let fd =
        let open Unix in
        match sockaddr with
        | ADDR_UNIX _ ->
            failwith "UNIX sockets should be bound and passed in with Use"
        | ADDR_INET (inet_addr, _) ->
            if is_inet6_addr inet_addr then Miou_unix.tcpv6 ()
            else Miou_unix.tcpv4 ()
      in
      Log.debug (fun m -> m "binding %a" pp_sockaddr sockaddr);
      Miou_unix.bind_and_listen ?backlog fd sockaddr;
      (fd, sockaddr)

let clear ?(parallel = true) ?stop ?(config = H1.Config.default) ?backlog ?ready
    ?error_handler:(user's_error_handler = default_error_handler) ?upgrade
    ~handler:user's_handler listen =
  let domains = Miou.Domain.available () in
  let closed = Atomic.make false in
  let call ~orphans fn =
    if parallel && domains >= 2 then ignore (Miou.call ~orphans fn)
    else ignore (Miou.async ~orphans fn)
  in
  let rec go orphans file_descr server'sockaddr =
    clean_up orphans;
    match accept_or_stop ?stop file_descr with
    | exception Unix.Unix_error ((Unix.EMFILE | Unix.ENFILE), _, _) ->
        Log.warn (fun m -> m "too many open files, backing off");
        Miou.yield ();
        go orphans file_descr server'sockaddr
    | None -> begin
        Log.debug (fun m ->
            m "stop the server on %a" pp_sockaddr server'sockaddr);
        Runtime.terminate orphans;
        if Atomic.compare_and_set closed false true then
          Miou_unix.close file_descr
      end
    | Some (fd', client'sockaddr) ->
        let socket = Miou_unix.to_file_descr fd' in
        inhibit (fun () -> Unix.setsockopt socket Unix.TCP_NODELAY true);
        Log.debug (fun m ->
            m "receive a connection from: %a" pp_sockaddr client'sockaddr);
        call ~orphans begin fun () ->
            http_1_1_server_connection ~config ~user's_error_handler ?upgrade
              ~user's_handler fd'
          end;
        go orphans file_descr server'sockaddr
  in
  let socket, server'sockaddr = listen_to_fd backlog listen in
  Option.iter (fun c -> ignore (Miou.Computation.try_return c ())) ready;
  go (Miou.orphans ()) socket server'sockaddr

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
    ?upgrade ~handler:user's_handler listen =
  let closed = Atomic.make false in
  let domains = Miou.Domain.available () in
  let call ~orphans fn =
    if parallel && domains >= 2 then ignore (Miou.call ~orphans fn)
    else ignore (Miou.async ~orphans fn)
  in
  let rec go orphans file_descr server'sockaddr =
    clean_up orphans;
    match accept_or_stop ?stop file_descr with
    | exception Unix.Unix_error ((Unix.EMFILE | Unix.ENFILE), _, _) ->
        Log.warn (fun m -> m "too many open files, backing off");
        Miou.yield ();
        go orphans file_descr server'sockaddr
    | None ->
        Log.debug (fun m ->
            m "Stopping service on %a" pp_sockaddr server'sockaddr);
        Runtime.terminate orphans;
        if Atomic.compare_and_set closed false true then
          Miou_unix.close file_descr
    | Some (fd', client'sockaddr) ->
        let socket = Miou_unix.to_file_descr fd' in
        inhibit (fun () -> Unix.setsockopt socket Unix.TCP_NODELAY true);
        let fn () =
          try
            let tls_flow = Tls_miou_unix.server_of_fd tls_config fd' in
            begin match (config, alpn tls_flow) with
            | `Both (_, h2), Some "h2" | `H2 h2, (Some "h2" | None) ->
                Log.debug (fun m ->
                    m "Start a h2 request handler for %a" pp_sockaddr
                      client'sockaddr);
                h2s_server_connection ~config:h2 ~user's_error_handler ?upgrade
                  ~user's_handler tls_flow
            | `Both (config, _), Some "http/1.1"
            | `HTTP_1_1 config, (Some "http/1.1" | None) ->
                Log.debug (fun m -> m "Start a http/1.1 request handler");
                https_1_1_server_connection ~config ~user's_error_handler
                  ?upgrade ~user's_handler tls_flow
            | `Both _, None ->
                failwith "No protocol specified during ALPN negotiation"
            | _, Some protocol ->
                Fmt.failwith "Unrecognized protocol: %S" protocol
            end
          with exn ->
            Log.err (fun m ->
                m "got a TLS error during the handshake: %s"
                  (Printexc.to_string exn));
            Miou_unix.close fd'
        in
        call ~orphans fn;
        go orphans file_descr server'sockaddr
  in
  let socket, server'sockaddr = listen_to_fd backlog listen in
  Log.debug (fun m ->
      m "Starting TLS service for %a" pp_sockaddr server'sockaddr);
  Option.iter (fun c -> ignore (Miou.Computation.try_return c ())) ready;
  go (Miou.orphans ()) socket server'sockaddr

module Websocket_connection = struct
  (* make it match Runtime.S signature *)
  include H1.Websocket.Server_connection

  let next_read_operation t =
    (next_read_operation t :> [ `Read | `Close | `Upgrade | `Yield ])

  let next_write_operation t =
    (next_write_operation t
      :> [ `Write of Bstr.t Faraday.iovec list
         | `Close of int
         | `Yield
         | `Upgrade ])

  let yield_reader _t _k = assert false

  let report_exn _t exn =
    Log.err (fun m -> m "websocket runtime: %s" (Printexc.to_string exn));
    raise exn
end

module D = Runtime.Make (TCP_and_H1) (Websocket_connection)
module E = Runtime.Make (Tls_miou_unix) (Websocket_connection)
module Bstream = Bstream
open H1
open H1.Websocket

module Ws_stop = struct
  (* TODO do we need the lock? re-use type stop? *)
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

  let on_close t fn =
    Miou.Mutex.protect t.lock @@ fun () ->
    while not ((t.received && t.emitted) || t.eof) do
      Miou.Condition.wait t.cond t.lock
    done;
    fn t.received t.emitted
end

module Websocket = struct
  type elt =
    [ `Connection_close
    | `Msg of H1.Websocket.Opcode.standard_non_control * bool
    | `Other
    | `Ping
    | `Pong ]
    * string

  type ic = unit -> elt option
  type oc = elt -> unit
  type stop = Ws_stop.t

  let stop () = Ws_stop.create ()
  let switch o = Ws_stop.set_eof o

  let write_websocket oc stop wsd =
    let rec go () =
      match Bstream.get oc with
      | None -> ()
      | Some (kind, data) ->
          begin match kind with
          | `Other -> failwith "Unsupported frame of kind `Other"
          | `Connection_close ->
              Wsd.close wsd; Bstream.close oc; Ws_stop.set_emmited stop; ()
          | `Ping -> Wsd.send_ping wsd
          | `Pong -> Wsd.send_pong wsd
          | `Msg (kind, is_fin) ->
              let len = String.length data in
              Wsd.send_bytes wsd ~kind ~is_fin
                (Bytes.unsafe_of_string data)
                ~off:0 ~len;
              ()
          end;
          go ()
    in
    go

  let websocket_handler src ic ivar stop wsd =
    ignore (Miou.Computation.try_return ivar wsd);
    let frame_handler ~opcode ~is_fin bstr ~off ~len =
      let data = Bstr.sub_string bstr ~off ~len in
      match opcode with
      | `Other _ -> Bstream.put ic (`Other, data)
      | `Connection_close ->
          Bstream.put ic (`Connection_close, data);
          Ws_stop.set_received stop;
          Logs.debug ~src (fun m -> m "+websocket: close");
          Bstream.close ic
      | #Opcode.standard_control as kind -> Bstream.put ic (kind, data)
      | #Opcode.standard_non_control as kind ->
          Bstream.put ic (`Msg (kind, is_fin), data)
    in
    let eof () =
      Logs.debug ~src (fun m -> m "+websocket: eof");
      Ws_stop.set_eof stop
    in
    { frame_handler; eof }

  let upgrade ?stop ~fn flow =
    (* TODO(upgrade) what size should it be? *)
    let ic = Bstream.create 0x100 in
    let oc = Bstream.create 0x100 in
    let ivar = Miou.Computation.create () in
    let stop =
      match stop with None -> Ws_stop.create () | Some stop -> stop
    in
    let tags = Logs.Tag.empty in
    let tags =
      match flow with
      | `Tcp flow ->
          let sockaddr = Unix.getpeername (Miou_unix.to_file_descr flow) in
          Logs.Tag.add peer (Fmt.str "ws:%a" pp_sockaddr sockaddr) tags
      | `Tls flow ->
          let flow = Tls_miou_unix.file_descr flow in
          let sockaddr = Unix.getpeername (Miou_unix.to_file_descr flow) in
          Logs.Tag.add peer (Fmt.str "wss:%a" pp_sockaddr sockaddr) tags
    in
    let websocket_handler = websocket_handler src ic ivar stop in
    let conn =
      Websocket.Server_connection.create ~websocket_handler
      (* wsd -> input_handlers *)
    in
    let runtime's_prm =
      match flow with
      | `Tcp flow -> D.run conn ~tags flow
      | `Tls flow -> E.run conn ~tags flow
    in
    let wsd = Miou.Computation.await_exn ivar in
    let writer = Miou.async (write_websocket oc stop wsd) in
    let user's_handler =
      Miou.async @@ fun () -> fn (fun () -> Bstream.get ic) (Bstream.put oc)
    in
    let close =
      Miou.async @@ fun () ->
      Ws_stop.on_close stop @@ fun received emitted ->
      if received && emitted then
        Logs.debug ~src (fun m -> m "Websocket closed properly")
      else begin
        Logs.debug ~src (fun m -> m "Websocket closed unproperly");
        (* we need to close the runtime's writer here
         [Wsd.close] close the writer and also write a close frame *)
        Wsd.close wsd;
        (* ic and oc may not have been closed
         we halt instead of close to disregard any data left on the streams *)
        Bstream.halt ic;
        Bstream.halt oc
      end
    in
    Miou.await_all [ runtime's_prm; user's_handler; writer; close ]
    |> List.iter (function Ok () -> () | Error exn -> raise exn)
end
