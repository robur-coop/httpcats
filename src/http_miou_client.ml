open Http_miou_unix

module TLS_for_httpaf = struct
  include TLS

  let shutdown flow _ = Miou_unix.disown flow.flow
end

module Httpaf_Client_connection = struct
  include Httpaf.Client_connection

  let yield_reader _ = assert false

  let next_read_operation t =
    (next_read_operation t :> [ `Close | `Read | `Yield ])
end

module A = Runtime.Make (TLS_for_httpaf) (Httpaf_Client_connection)
module B = Runtime.Make (TCP) (Httpaf_Client_connection)
module C = Runtime.Make (TLS) (H2.Client_connection)
module D = Runtime.Make (TCP) (H2.Client_connection)

type config = [ `V1 of Httpaf.Config.t | `V2 of H2.Config.t ]
type flow = [ `Tls of TLS.t | `Tcp of Miou_unix.file_descr ]
type request = [ `V1 of Httpaf.Request.t | `V2 of H2.Request.t ]
type response = [ `V1 of Httpaf.Response.t | `V2 of H2.Response.t ]

type 'body body =
  { body : 'body
  ; write_string : 'body -> ?off:int -> ?len:int -> string -> unit
  ; close : 'body -> unit
  ; release : unit -> unit
  }

type ('resp, 'body) version =
  | V1 : (Httpaf.Response.t, [ `write ] Httpaf.Body.t body) version
  | V2 : (H2.Response.t, H2.Body.Writer.t body) version

type error =
  [ `V1 of Httpaf.Client_connection.error
  | `V2 of H2.Client_connection.error
  | `Protocol of string ]

let pp_error ppf = function
  | `V1 (`Malformed_response msg) ->
      Fmt.pf ppf "Malformed HTTP/1.1 response: %s" msg
  | `V1 (`Invalid_response_body_length _resp) ->
      Fmt.pf ppf "Invalid response body length"
  | `V1 (`Exn exn) | `V2 (`Exn exn) ->
      Fmt.pf ppf "Got an unexpected exception: %S" (Printexc.to_string exn)
  | `V2 (`Malformed_response msg) -> Fmt.pf ppf "Malformed H2 response: %s" msg
  | `V2 (`Invalid_response_body_length _resp) ->
      Fmt.pf ppf "Invalid response body length"
  | `V2 (`Protocol_error (err, msg)) ->
      Fmt.pf ppf "Protocol error %a: %s" H2.Error_code.pp_hum err msg
  | `Protocol msg -> Fmt.string ppf msg

type ('resp, 'acc) await = unit -> ('resp * 'acc, error) result

type 'acc process =
  | Process :
      ('resp, 'body) version * ('resp, 'acc) await * 'body
      -> 'acc process

let src = Logs.Src.create "http-miou-client"

module Log = (val Logs.src_log src : Logs.LOG)

(* NOTE(dinosaure): we avoid first-class module here. *)
let run ~f acc config flow request =
  let response : response option ref = ref None
  and error = ref None
  and acc = ref acc in
  let error_handler err =
    Log.err (fun m -> m "got an error: %a" pp_error err);
    match err with
    | `V1 (`Exn (Runtime.Flow msg)) | `V2 (`Exn (Runtime.Flow msg)) ->
        error := Some (`Protocol msg)
    | err -> error := Some err
  in
  let response_handler ?(shutdown = Fun.const ()) = function
    | `V1 (resp, body) ->
        let rec on_eof = shutdown
        and on_read bstr ~off ~len =
          let str = Bigstringaf.substring bstr ~off ~len in
          acc := f (`V1 resp) !acc str;
          Httpaf.Body.schedule_read body ~on_read ~on_eof
        in
        response := Some (`V1 resp);
        Httpaf.Body.schedule_read body ~on_read ~on_eof
    | `V2 (resp, body) ->
        let rec on_eof = shutdown
        and on_read bstr ~off ~len =
          let str = Bigstringaf.substring bstr ~off ~len in
          acc := f (`V2 resp) !acc str;
          H2.Body.Reader.schedule_read body ~on_read ~on_eof
        in
        response := Some (`V2 resp);
        H2.Body.Reader.schedule_read body ~on_read ~on_eof
  in
  let give =
    match flow with
    | `Tls flow -> [ Miou_unix.owner flow.TLS.flow ]
    | `Tcp flow -> [ Miou_unix.owner flow ]
  in
  match (flow, config, request) with
  | `Tls flow, `V1 config, `V1 request ->
      let read_buffer_size = config.Httpaf.Config.read_buffer_size in
      let disown flow = Miou_unix.disown flow.TLS.flow in
      let response_handler resp body = response_handler (`V1 (resp, body)) in
      let error_handler error = error_handler (`V1 error) in
      let body, conn =
        Httpaf.Client_connection.request ~config request ~error_handler
          ~response_handler
      in
      let orphans = Miou.orphans () in
      let { Runtime.protect }, prm, close =
        Log.debug (fun m -> m "start an http/1.1 request over TLS");
        A.run conn ~give ~disown ~read_buffer_size flow
      in
      let await () =
        match (Miou.await prm, !error, !response) with
        | _, Some error, _ -> Error error
        | Error exn, _, _ -> Error (`V1 (`Exn exn))
        | Ok (), None, Some (`V1 response) -> Ok (response, !acc)
        | Ok (), None, (Some (`V2 _) | None) -> assert false
      in
      let release () =
        Runtime.terminate orphans;
        close ()
      in
      let write_string body ?off ?len str =
        protect ~orphans (Httpaf.Body.write_string body ?off ?len) str
      in
      let close body = protect ~orphans Httpaf.Body.close_writer body in
      let body = { body; write_string; close; release } in
      Process (V1, await, body)
  | `Tcp flow, `V1 config, `V1 request ->
      let read_buffer_size = config.Httpaf.Config.read_buffer_size in
      let disown = Miou_unix.disown in
      let response_handler resp body = response_handler (`V1 (resp, body)) in
      let error_handler error = error_handler (`V1 error) in
      let body, conn =
        Httpaf.Client_connection.request ~config request ~error_handler
          ~response_handler
      in
      let orphans = Miou.orphans () in
      let { Runtime.protect }, prm, close =
        B.run conn ~give ~disown ~read_buffer_size flow
      in
      let await () =
        match (Miou.await prm, !error, !response) with
        | _, Some error, _ -> Error error
        | Error exn, _, _ -> Error (`V1 (`Exn exn))
        | Ok (), None, Some (`V1 response) -> Ok (response, !acc)
        | Ok (), None, (Some (`V2 _) | None) -> assert false
      in
      let release () =
        Runtime.terminate orphans;
        close ()
      in
      let write_string body ?off ?len str =
        protect ~orphans (Httpaf.Body.write_string body ?off ?len) str
      in
      let close body = protect ~orphans Httpaf.Body.close_writer body in
      let body = { body; write_string; close; release } in
      Process (V1, await, body)
  | `Tls flow, `V2 config, `V2 request ->
      let read_buffer_size = config.H2.Config.read_buffer_size in
      let disown flow = Miou_unix.disown flow.TLS.flow in
      let error_handler error = error_handler (`V2 error) in
      let conn = H2.Client_connection.create ~config ~error_handler () in
      let shutdown () = H2.Client_connection.shutdown conn in
      let response_handler resp body =
        response_handler ~shutdown (`V2 (resp, body))
      in
      let body =
        H2.Client_connection.request conn request ~error_handler
          ~response_handler
      in
      let orphans = Miou.orphans () in
      let { Runtime.protect }, prm, close =
        Log.debug (fun m -> m "start an h2 request over TLS");
        C.run conn ~give ~disown ~read_buffer_size flow
      in
      let await () =
        match (Miou.await prm, !error, !response) with
        | _, Some error, _ -> Error error
        | Error exn, _, _ -> Error (`V2 (`Exn exn))
        | Ok (), None, Some (`V2 response) -> Ok (response, !acc)
        | Ok (), None, (Some (`V1 _) | None) -> assert false
      in
      let release () =
        Runtime.terminate orphans;
        close ()
      in
      let write_string body ?off ?len str =
        protect ~orphans (H2.Body.Writer.write_string body ?off ?len) str
      in
      let close body =
        Log.debug (fun m -> m "close the stream from the application level");
        protect ~orphans H2.Body.Writer.close body
      in
      let body = { body; write_string; close; release } in
      Process (V2, await, body)
  | `Tcp flow, `V2 config, `V2 request ->
      let read_buffer_size = config.H2.Config.read_buffer_size in
      let disown = Miou_unix.disown in
      let error_handler error = error_handler (`V2 error) in
      let conn = H2.Client_connection.create ~config ~error_handler () in
      let shutdown () = H2.Client_connection.shutdown conn in
      let response_handler resp body =
        response_handler ~shutdown (`V2 (resp, body))
      in
      let body =
        H2.Client_connection.request conn request ~error_handler
          ~response_handler
      in
      let orphans = Miou.orphans () in
      let { Runtime.protect }, prm, close =
        D.run conn ~give ~disown ~read_buffer_size flow
      in
      let await () =
        match (Miou.await prm, !error, !response) with
        | _, Some error, _ -> Error error
        | Error exn, _, _ -> Error (`V2 (`Exn exn))
        | Ok (), None, Some (`V2 response) -> Ok (response, !acc)
        | Ok (), None, (Some (`V1 _) | None) -> assert false
      in
      let release () =
        Runtime.terminate orphans;
        close ()
      in
      let write_string body ?off ?len str =
        protect ~orphans (H2.Body.Writer.write_string body ?off ?len) str
      in
      let close body = protect ~orphans H2.Body.Writer.close body in
      let body = { body; write_string; close; release } in
      Process (V2, await, body)
  | _ -> Fmt.invalid_arg "Http_miou_unix.run: incompatible arguments"
