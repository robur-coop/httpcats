let src = Logs.Src.create "examples/websocket.ml"

module Log = (val Logs.src_log src : Logs.LOG)
open H1_ws

let write_loop ~out_stream wsd =
  let write =
   fun () ->
    match Bstream.get out_stream with
    | None -> `Stop
    | Some (kind, data) -> (
        match kind with
        | `Connection_close -> `Stop
        | `Ping -> Wsd.send_ping wsd; `Continue
        | `Pong -> Wsd.send_pong wsd; `Continue
        | `Other -> failwith "unsuported frame of kind `Other"
        | `Msg (kind, is_fin) ->
            Log.debug (fun m ->
                m "write loop: `Msg (%a, is_fin=%b)" Websocket.Opcode.pp_hum
                  (kind :> Websocket.Opcode.t)
                  is_fin);
            let len = Bytes.length data in
            Wsd.send_bytes wsd ~kind ~is_fin data ~off:0 ~len;
            `Continue)
  in
  let rec loop () =
    match write () with `Stop -> Wsd.close wsd; () | `Continue -> loop ()
  in
  loop ()

let handler ~orphans ~in_stream ~out_stream wsd =
  Log.debug (fun m -> m "Websocket.handler");
  let v =
    let frame_handler ~opcode ~is_fin bstr ~off ~len =
      let data =
        let s = Bigstringaf.substring bstr ~off ~len in
        String.to_bytes s
      in
      let v =
        match opcode with
        | `Other _ -> (`Other, data)
        | #Websocket.Opcode.standard_control as kind -> (kind, data)
        | #Websocket.Opcode.standard_non_control as kind ->
            (`Msg (kind, is_fin), data)
      in
      Bstream.put in_stream (Some v)
    in
    let eof () =
      Log.debug (fun m -> m "eof");
      Bstream.put in_stream None
    in
    Websocket.{ frame_handler; eof }
  in
  let () =
    ignore @@ Miou.async ~orphans @@ fun () -> write_loop ~out_stream wsd
  in
  v
