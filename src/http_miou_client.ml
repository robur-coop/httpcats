let src = Logs.Src.create "http-miou-client"

module Log = (val Logs.src_log src : Logs.LOG)
open Http_miou_unix

module H1_Client_connection = struct
  include H1.Client_connection

  let yield_reader _ = assert false

  let next_read_operation t =
    (next_read_operation t :> [ `Close | `Read | `Yield | `Upgrade ])

  let next_write_operation t =
    (next_write_operation t
      :> [ `Close of int
         | `Write of Bigstringaf.t Faraday.iovec list
         | `Yield
         | `Upgrade ])
end

module H2_Client_connection = struct
  include H2.Client_connection

  let next_read_operation t =
    (next_read_operation t :> [ `Close | `Read | `Yield | `Upgrade ])

  let next_write_operation t =
    (next_write_operation t
      :> [ `Close of int
         | `Write of Bigstringaf.t Faraday.iovec list
         | `Yield
         | `Upgrade ])
end

module A = Runtime.Make (TLS) (H1_Client_connection)
module B = Runtime.Make (TCP) (H1_Client_connection)
module C = Runtime.Make (TLS) (H2_Client_connection)
module D = Runtime.Make (TCP) (H2_Client_connection)

type config = [ `V1 of H1.Config.t | `V2 of H2.Config.t ]
type flow = [ `Tls of Tls_miou_unix.t | `Tcp of Miou_unix.file_descr ]
type request = [ `V1 of H1.Request.t | `V2 of H2.Request.t ]
type response = [ `V1 of H1.Response.t | `V2 of H2.Response.t ]

type error =
  [ `V1 of H1.Client_connection.error
  | `V2 of H2.Client_connection.error
  | `Protocol of string
  | `Exn of exn ]

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
  | `Exn exn -> Fmt.pf ppf "%S" (Printexc.to_string exn)

type ('resp, 'body) version =
  | V1 : (H1.Response.t, H1.Body.Writer.t) version
  | V2 : (H2.Response.t, H2.Body.Writer.t) version

exception Error of error

let empty = Printexc.get_callstack 0

type 'acc process =
  | Process : {
        version: ('resp, 'body) version
      ; acc: 'acc ref
      ; response: 'resp Miou.Computation.t
      ; body: 'body
      ; process: unit Miou.t
    }
      -> 'acc process

let http_1_1_response_handler ~f acc =
  let acc = ref acc in
  let response = Miou.Computation.create () in
  let response_handler resp body =
    ignore (Miou.Computation.try_return response resp);
    let rec on_eof () = H1.Body.Reader.close body
    and on_read bstr ~off ~len =
      let str = Bigstringaf.substring bstr ~off ~len in
      acc := f (`V1 resp) !acc str;
      H1.Body.Reader.schedule_read body ~on_read ~on_eof
    in
    H1.Body.Reader.schedule_read body ~on_read ~on_eof
  in
  (response_handler, response, acc)

let http_1_1_error_handler response err =
  let err =
    match err with `Exn (Runtime.Flow msg) -> `Protocol msg | err -> `V1 err
  in
  ignore (Miou.Computation.try_cancel response (Error err, empty))

let h2_response_handler conn ~f response acc =
  let acc = ref acc in
  let response_handler resp body =
    ignore (Miou.Computation.try_return response resp);
    let rec on_eof () =
      H2.Body.Reader.close body;
      H2.Client_connection.shutdown conn
    and on_read bstr ~off ~len =
      let str = Bigstringaf.substring bstr ~off ~len in
      acc := f (`V2 resp) !acc str;
      H2.Body.Reader.schedule_read body ~on_read ~on_eof
    in
    H2.Body.Reader.schedule_read body ~on_read ~on_eof
  in
  (response_handler, acc)

let h2_error_handler response err =
  let err =
    match err with `Exn (Runtime.Flow msg) -> `Protocol msg | err -> `V2 err
  in
  ignore (Miou.Computation.try_cancel response (Error err, empty))

let pp_request ppf (flow, request) =
  match (flow, request) with
  | `Tls _, `V1 _ -> Fmt.string ppf "http/1.1 + tls"
  | `Tcp _, `V1 _ -> Fmt.string ppf "http/1.1"
  | `Tls _, `V2 _ -> Fmt.string ppf "h2 + tls"
  | `Tcp _, `V2 _ -> Fmt.string ppf "h2"

let run ~f acc config flow request =
  Log.debug (fun m -> m "start a new %a request" pp_request (flow, request));
  match (flow, config, request) with
  | `Tls flow, `V1 config, `V1 request ->
      let read_buffer_size = config.H1.Config.read_buffer_size in
      let response_handler, response, acc = http_1_1_response_handler ~f acc in
      let error_handler = http_1_1_error_handler response in
      let body, conn =
        H1.Client_connection.request ~config request ~error_handler
          ~response_handler
      in
      let prm = A.run conn ~read_buffer_size flow in
      Process { version= V1; acc; response; body; process= prm }
  | `Tcp flow, `V1 config, `V1 request ->
      let read_buffer_size = config.H1.Config.read_buffer_size in
      let response_handler, response, acc = http_1_1_response_handler ~f acc in
      let error_handler = http_1_1_error_handler response in
      let body, conn =
        H1.Client_connection.request ~config request ~error_handler
          ~response_handler
      in
      let prm = B.run conn ~read_buffer_size flow in
      Process { version= V1; acc; response; body; process= prm }
  | `Tls flow, `V2 config, `V2 request ->
      let read_buffer_size = config.H2.Config.read_buffer_size in
      let response = Miou.Computation.create () in
      let error_handler = h2_error_handler response in
      let conn = H2.Client_connection.create ~config ~error_handler () in
      let response_handler, acc = h2_response_handler conn ~f response acc in
      let body =
        H2.Client_connection.request conn ~error_handler ~response_handler
          request
      in
      let prm = C.run conn ~read_buffer_size flow in
      Process { version= V2; acc; response; body; process= prm }
  | `Tcp flow, `V2 config, `V2 request ->
      let read_buffer_size = config.H2.Config.read_buffer_size in
      let response = Miou.Computation.create () in
      let error_handler = h2_error_handler response in
      let conn = H2.Client_connection.create ~config ~error_handler () in
      let response_handler, acc = h2_response_handler conn ~f response acc in
      let body =
        H2.Client_connection.request conn ~error_handler ~response_handler
          request
      in
      let prm = D.run conn ~read_buffer_size flow in
      Process { version= V2; acc; response; body; process= prm }
  | _ -> invalid_arg "Http_miou_client.run"
