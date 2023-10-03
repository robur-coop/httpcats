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
    | `Closed -> Fmt.string ppf "Connection closed by peer"

  type state = [ `Active of Tls.Engine.state | `End_of_input | `Error of error ]

  type t = {
    role : [ `Server | `Client ];
    flow : Flow.t;
    mutable state : state;
    mutable linger : Cstruct.t list;
    mutable writer_closed : bool;
  }

  let tls_alert alert = `Error (`Tls_alert alert)
  let tls_fail failure = `Error (`Tls_failure failure)

  let lift_read_result = function
    | Ok ((`Data _ | `End_of_input) as x) -> x
    | Error err -> `Error (`Read err)

  let lift_write_result = function
    | Ok () -> `Ok ()
    | Error err -> `Error (`Write err)

  let check_write flow res =
    let () =
      match (flow.state, lift_write_result res) with
      | `Active _, ((`End_of_input | `Error _) as err) ->
          flow.state <- err;
          Log.warn (fun m -> m "close the socket due to a writing error");
          Flow.close flow.flow
      | _ -> ()
    in
    match res with Ok () -> Ok () | Error err -> Error (`Write err)

  let read_react ~read_buffer_size flow =
    let handle tls buf =
      match Tls.Engine.handle_tls tls buf with
      | Ok (res, `Response resp, `Data data) ->
          let state =
            match res with
            | `Ok tls -> `Active tls
            | `Eof -> `End_of_input
            | `Alert alert -> tls_alert alert
          in
          flow.state <- state;
          let _ =
            match resp with
            | None -> Ok ()
            | Some buf -> check_write flow (Flow.writev flow.flow [ buf ])
          in
          (*
          NOTE(dinosaure): with the /shutdown/ thing, we should not close
          the connection and let the user to do so.

          let () =
            match res with
            | `Ok _ -> ()
            | _ ->
                Log.warn (fun m -> m "close the socket due to a reading error");
                Flow.close flow.flow
          in
          *)
          let data =
            match data with
            | None -> None
            | Some data when Cstruct.length data > read_buffer_size ->
                let data, linger = Cstruct.split data read_buffer_size in
                flow.linger <- linger :: flow.linger;
                Some data
            | Some data -> Some data
          in
          `Ok data
      | Error (failure, `Response resp) ->
          let reason = tls_fail failure in
          flow.state <- reason;
          let _ = Flow.writev flow.flow [ resp ] in
          Log.warn (fun m -> m "close the socket due to a reading error");
          Flow.close flow.flow;
          reason
    in
    match flow.state with
    | (`End_of_input | `Error _) as err -> err
    | `Active _ -> (
        match lift_read_result (Flow.read ~read_buffer_size flow.flow) with
        | (`End_of_input | `Error _) as v ->
            flow.state <- v;
            v
        | `Data buf -> (
            match flow.state with
            | `Active tls -> handle tls buf
            | (`End_of_input | `Error _) as v -> v))

  let split ~len cs =
    if Cstruct.length cs >= len then Cstruct.split cs len
    else (cs, Cstruct.empty)

  let rec read ?(read_buffer_size = 0x1000) flow =
    match flow.linger with
    | _ :: _ as bufs ->
        let cs = Cstruct.concat (List.rev bufs) in
        let data, linger = split ~len:read_buffer_size cs in
        if Cstruct.length linger > 0 then flow.linger <- [ linger ]
        else flow.linger <- [];
        Ok (`Data data)
    | [] -> (
        match read_react ~read_buffer_size flow with
        | `Ok None -> read ~read_buffer_size flow
        | `Ok (Some buf) -> Ok (`Data buf)
        | `End_of_input -> Ok `End_of_input
        | `Error e -> Error e)

  let writev flow bufs =
    if flow.writer_closed then Error `Closed
    else
      match flow.state with
      | `End_of_input -> Error `Closed
      | `Error e -> Error e
      | `Active tls -> (
          match Tls.Engine.send_application_data tls bufs with
          | Some (tls, answer) ->
              flow.state <- `Active tls;
              check_write flow (Flow.writev flow.flow [ answer ])
          | None -> assert false)

  let write flow buf = writev flow [ buf ]

  let rec drain_handshake flow =
    match flow.state with
    | `Active tls when not (Tls.Engine.handshake_in_progress tls) -> Ok flow
    | _ -> (
        match read_react ~read_buffer_size:0x1000 flow with
        | `Ok mbuf ->
            flow.linger <- Option.to_list mbuf @ flow.linger;
            drain_handshake flow
        | `Error e -> Error e
        | `End_of_input -> Error `Closed)

  let close flow =
    match flow.state with
    | `Active tls ->
        flow.state <- `End_of_input;
        let _, buf = Tls.Engine.send_close_notify tls in
        let _ = Flow.writev flow.flow [ buf ] in
        Log.debug (fun m -> m "close the socket");
        Flow.close flow.flow
    | _ -> ()

  let shutdown flow v =
    match (flow.state, v) with
    | `Active tls, `Send when not flow.writer_closed ->
        let tls, buf = Tls.Engine.send_close_notify tls in
        flow.state <- `Active tls;
        flow.writer_closed <- true;
        let _ = Flow.writev flow.flow [ buf ] in
        Flow.shutdown flow.flow `Send
    | `Active _, _ -> close flow
    | _ -> ()

  let client_of_flow conf ?host flow =
    let conf' =
      match host with None -> conf | Some host -> Tls.Config.peer conf host
    in
    let tls, init = Tls.Engine.client conf' in
    let tls_flow =
      {
        role = `Client;
        flow;
        state = `Active tls;
        linger = [];
        writer_closed = false;
      }
    in
    match check_write tls_flow (Flow.writev flow [ init ]) with
    | Ok () -> drain_handshake tls_flow
    | Error _ as err -> err
end
