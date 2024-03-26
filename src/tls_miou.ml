let src = Logs.Src.create "tls-miou"

module Log = (val Logs.src_log src : Logs.LOG)

module Make (Flow : Flow.S) = struct
  type error =
    [ `Tls_alert of Tls.Packet.alert_type
    | `Tls_failure of Tls.Engine.failure
    | `Read of Flow.error
    | `Write of Flow.error
    | `Closed ]

  let pp_error ppf = function
    | `Tls_failure failure -> Tls.Engine.pp_failure ppf failure
    | `Tls_alert alert -> Fmt.string ppf (Tls.Packet.alert_type_to_string alert)
    | `Read err -> Flow.pp_error ppf err
    | `Write err -> Flow.pp_error ppf err
    | `Closed -> Fmt.string ppf "Connection closed by peer (tls)"

  type state =
    [ `Active of Tls.Engine.state
    | `Read_closed of Tls.Engine.state
    | `Write_closed of Tls.Engine.state
    | `Closed
    | `Error of error ]

  type t = {
      role: [ `Server | `Client ]
    ; flow: Flow.t
    ; mutable state: state
    ; mutable linger: Cstruct.t list
    ; mutable rest: Cstruct.t list
    ; read_buffer_size: int
  }

  let half_close state mode =
    match (state, mode) with
    | `Active tls, `read -> `Read_closed tls
    | `Active tls, `write -> `Write_closed tls
    | `Active _, `read_write -> `Closed
    | `Read_closed tls, `read -> `Read_closed tls
    | `Read_closed _, (`write | `read_write) -> `Closed
    | `Write_closed tls, `write -> `Write_closed tls
    | `Write_closed _, (`read | `read_write) -> `Closed
    | ((`Closed | `Error _) as e), (`read | `write | `read_write) -> e

  let inject_state tls = function
    | `Active _ -> `Active tls
    | `Read_closed _ -> `Read_closed tls
    | `Write_closed _ -> `Write_closed tls
    | (`Closed | `Error _) as e -> e

  let tls_alert a = `Error (`Tls_alert a)
  let tls_fail f = `Error (`Tls_failure f)

  let write_flow flow buf =
    match Flow.writev flow.flow [ buf ] with
    | Ok _ as o -> o
    | Error `Closed ->
        flow.state <- half_close flow.state `write;
        Error `Closed
    | Error e ->
        flow.state <- `Error (`Write e);
        Error (`Write e)

  let handle flow tls buf =
    match Tls.Engine.handle_tls tls buf with
    | Ok (state, eof, `Response resp, `Data data) ->
        let state = inject_state state flow.state in
        let state = Option.(value ~default:state (map (fun `Eof -> half_close state `read) eof)) in
        flow.state <- state;
        let _ = match resp with
          | None -> Ok ()
          | Some buf -> write_flow flow buf in
        `Ok data
    | Error (fail, `Response resp) ->
        let reason = match fail with
          | `Alert a -> tls_alert a | f -> tls_fail f in
        flow.state <- reason;
        let _ = Flow.writev flow.flow [resp] in
        reason
  [@@ocamlformat "disable"]

  let read flow =
    let buf = Bytes.create flow.read_buffer_size in
    match Flow.read flow.flow buf ~off:0 ~len:(Bytes.length buf) with
    | Error _ as err -> err
    | Ok 0 -> Ok `Eof
    | Ok len -> Ok (`Data (Cstruct.of_bytes buf ~off:0 ~len))

  let read_react flow =
    match flow.state with
    | `Error _ as e -> e
    | `Read_closed _ -> `Eof
    | `Closed -> `Eof (* XXX(dinosaure): ECONNRESET? *)
    | `Active _ | `Write_closed _ ->
      match read flow with
      | Error e ->
        flow.state <- `Error (`Read e);
        `Error (`Read e)
      | Ok `Eof ->
        flow.state <- half_close flow.state `read;
        `Eof
      | Ok `Data buf ->
        match flow.state with
        | `Active tls | `Write_closed tls -> handle flow tls buf
        | `Read_closed _ -> `Eof
        | `Closed -> `Error (`Write `Closed)
        | `Error _ as e -> e
  [@@ocamlformat "disable"]

  let rec read_in flow =
    match flow.linger with
    | _ :: _ as bufs ->
        flow.linger <- [];
        Ok (`Data (Cstruct.concat @@ List.rev bufs))
    | [] -> (
        match read_react flow with
        | `Ok None -> read_in flow
        | `Ok (Some buf) -> Ok (`Data buf)
        | `Eof -> Ok `Eof
        | `Error e -> Error e)

  let writev flow bufs =
    match flow.state with
    | `Closed | `Write_closed _ -> Error `Closed
    | `Error e -> Error (e :> error)
    | `Active tls | `Read_closed tls -> (
        match Tls.Engine.send_application_data tls bufs with
        | Some (tls, answer) ->
            flow.state <- `Active tls;
            write_flow flow answer
        | None -> assert false)

  let write flow buf = writev flow [ buf ]

  let rec drain_handshake flow =
    Log.debug (fun m -> m "drain the handshake");
    match flow.state with
    | `Active tls when not (Tls.Engine.handshake_in_progress tls) ->
        flow.rest <- flow.linger;
        flow.linger <- [];
        Ok flow
    | _ -> (
        match read_react flow with
        | `Ok mbuf ->
            flow.linger <- Option.to_list mbuf @ flow.linger;
            drain_handshake flow
        | `Error e -> Error (e :> error)
        | `Eof -> Error `Closed)

  let close flow =
    let () =
      match flow.state with
      | `Active tls | `Read_closed tls ->
          let tls, buf = Tls.Engine.send_close_notify tls in
          flow.state <- inject_state tls flow.state;
          flow.state <- `Closed;
          let _ = write_flow flow buf in
          ()
      | `Write_closed _ -> flow.state <- `Closed
      | _ -> ()
    in
    Flow.close flow.flow

  let shutdown flow mode =
    match flow.state with
    | `Active tls | `Read_closed tls | `Write_closed tls ->
        let tls, buf =
          match (flow.state, mode) with
          | (`Active tls | `Read_closed tls), (`write | `read_write) ->
              let tls, buf = Tls.Engine.send_close_notify tls in
              (tls, Some buf)
          | _, _ -> (tls, None)
        in
        flow.state <- inject_state tls (half_close flow.state mode);
        Option.fold ~none:()
          ~some:(fun b ->
            let _ = write_flow flow b in
            ())
          buf;
        if flow.state = `Closed then Flow.close flow.flow
    | `Error _ | `Closed -> Flow.close flow.flow

  let client_of_flow conf ?(read_buffer_size = 0x1000) ?host flow =
    let conf' =
      match host with None -> conf | Some host -> Tls.Config.peer conf host
    in
    let tls, init = Tls.Engine.client conf' in
    let tls_flow =
      {
        role= `Client
      ; flow
      ; state= `Active tls
      ; linger= []
      ; rest= []
      ; read_buffer_size
      }
    in
    match write_flow tls_flow init with
    | Ok () -> drain_handshake tls_flow
    | Error err -> Error err

  let server_of_flow conf ?(read_buffer_size = 0x1000) flow =
    let tls = Tls.Engine.server conf in
    let tls_flow =
      {
        role= `Server
      ; flow
      ; state= `Active tls
      ; linger= []
      ; rest= []
      ; read_buffer_size
      }
    in
    drain_handshake tls_flow

  let read t buf ~off ~len =
    match t.rest with
    | cs :: rest ->
      let len' = min (Cstruct.length cs) len in
      Log.debug (fun m -> m "transmit some saved bytes (%d byte(s))" len');
      Cstruct.blit_to_bytes cs 0 buf off len';
      let cs = Cstruct.shift cs len' in
      if Cstruct.length cs = 0 then t.rest <- rest
      else t.rest <- cs :: rest;
      Ok len'
    | [] ->
      Log.debug (fun m -> m "start to read some bytes from the underlying flow");
      match read_in t with
      | Ok `Eof -> Ok 0
      | Ok (`Data cs) ->
        let len = min (Cstruct.length cs) len in
        Cstruct.blit_to_bytes cs 0 buf off len;
        if Cstruct.length cs > len
        then t.rest <- [ Cstruct.shift cs len ];
        Ok len
      | Error _ as err -> err
  [@@ocamlformat "disable"]
end
