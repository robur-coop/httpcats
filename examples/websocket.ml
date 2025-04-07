let src = Logs.Src.create "examples/websocket.ml"

module Log = (val Logs.src_log src : Logs.LOG)
open H1_ws

let write_loop ~out_stream wsd =
  let write =
    let current_message = ref [] in
    fun () ->
      match Bstream.get out_stream with
      | None -> `Stop
      | Some (kind, data) -> (
          match kind with
          | `Close -> `Stop
          | `Ping -> Wsd.send_ping wsd; `Continue
          | `Pong -> Wsd.send_pong wsd; `Continue
          | `Other -> failwith "unsuported frame of kind `Other"
          | `Data (kind, is_fin) -> (
              match is_fin with
              | false ->
                  current_message := data :: !current_message;
                  `Continue
              | true ->
                  let data =
                    Bytes.concat Bytes.empty
                      (List.rev (data :: !current_message))
                  in
                  current_message := [];
                  let len = Bytes.length data in
                  Wsd.send_bytes wsd ~kind data ~off:0 ~len;
                  `Continue))
  in
  let rec loop () =
    match write () with
    | `Stop ->
        Log.debug (fun m -> m "write loop stop");
        Wsd.close wsd;
        ()
    | `Continue ->
        Log.debug (fun m -> m "write loop continue");
        loop ()
  in
  loop ()

let handler ~orphans ~in_stream ~out_stream wsd =
  Log.debug (fun m -> m "Websocket.handler");
  let v =
    let message_is_binary = ref `Binary in
    let frame_handler ~opcode ~is_fin bstr ~off ~len =
      let data =
        let s = Bigstringaf.substring bstr ~off ~len in
        String.to_bytes s
      in
      Bstream.put in_stream
      @@
      match opcode with
      | `Connection_close -> Some (`Close, data)
      | `Ping -> Some (`Ping, data)
      | `Pong -> Some (`Pong, data)
      | `Other _ -> Some (`Other, data)
      | `Text ->
          message_is_binary := `Text;
          Some (`Data (`Text, is_fin), data)
      | `Binary ->
          message_is_binary := `Binary;
          Some (`Data (`Binary, is_fin), data)
      | `Continuation -> Some (`Data (!message_is_binary, is_fin), data)
    in
    let eof () =
      Log.debug (fun m -> m "eof");
      Bstream.put in_stream None
    in
    Websocket_connection.{ frame_handler; eof }
  in
  let () =
    ignore @@ Miou.async ~orphans @@ fun () -> write_loop ~out_stream wsd
  in
  v
