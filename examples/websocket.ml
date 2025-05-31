let src = Logs.Src.create "examples/websocket.ml"

module Log = (val Logs.src_log src : Logs.LOG)
open H1_ws

let write oc ivar =
  let wsd = Miou.Computation.await_exn ivar in
  let write () =
    match Bstream.get oc with
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
  let rec go () =
    match write () with `Stop -> Wsd.close wsd; () | `Continue -> go ()
  in
  go

let _handler ~fn _wsd =
  let ic = Bstream.create 0x100 None in
  let oc = Bstream.create 0x100 None in
  let input_handlers =
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
      Bstream.put ic (Some v)
    in
    let eof () = Bstream.put ic None in
    Websocket.{ frame_handler; eof }
  in
  let ivar = Miou.Computation.create () in
  let prm0 =
    Miou.async @@ fun () ->
    let reader = Miou.async @@ fun () -> fn ic oc in
    let writer = Miou.async (write oc ivar) in
    Miou.await_all [ reader; writer ]
    |> List.iter (function Ok () -> () | Error exn -> raise exn)
  in
  ( prm0
  , fun wsd ->
      ignore (Miou.Computation.try_return ivar wsd);
      input_handlers )
