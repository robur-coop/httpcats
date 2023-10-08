(*----------------------------------------------------------------------------
    Copyright (c) 2018 Inhabited Type LLC.
    Copyright (c) 2018 Anton Bachin
    Copyright (c) 2023 Robur

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

module type RUNTIME = sig
  type t

  val next_read_operation : t -> [ `Read | `Yield | `Close ]
  val read : t -> Bigstringaf.t -> off:int -> len:int -> int
  val read_eof : t -> Bigstringaf.t -> off:int -> len:int -> int
  val yield_reader : t -> (unit -> unit) -> unit

  val next_write_operation :
    t -> [ `Write of Bigstringaf.t Faraday.iovec list | `Close of int | `Yield ]

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  val yield_writer : t -> (unit -> unit) -> unit
  val report_exn : t -> exn -> unit
end

module Buffer : sig
  type t

  val create : int -> t
  val get : t -> f:(Bigstringaf.t -> off:int -> len:int -> int) -> int
  val put : t -> f:(Bigstringaf.t -> off:int -> len:int -> int) -> int
end = struct
  type t = {
    mutable buffer : Bigstringaf.t;
    mutable off : int;
    mutable len : int;
  }

  let create size =
    let buffer = Bigstringaf.create size in
    { buffer; off = 0; len = 0 }

  let compress t =
    if t.len = 0 then begin
      t.off <- 0;
      t.len <- 0
    end
    else if t.off > 0 then begin
      Bigstringaf.blit t.buffer ~src_off:t.off t.buffer ~dst_off:0 ~len:t.len;
      t.off <- 0
    end

  let get t ~f =
    let n = f t.buffer ~off:t.off ~len:t.len in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0 then t.off <- 0;
    n

  let put t ~f =
    compress t;
    let off = t.off + t.len in
    let buf = t.buffer in
    if Bigstringaf.length buf = t.len then begin
      t.buffer <- Bigstringaf.create (2 * Bigstringaf.length buf);
      Bigstringaf.blit buf ~src_off:t.off t.buffer ~dst_off:0 ~len:t.len
    end;
    let n = f t.buffer ~off ~len:(Bigstringaf.length t.buffer - off) in
    t.len <- t.len + n;
    n
end

let src = Logs.Src.create "http-miou-unix"

module Log = (val Logs.src_log src : Logs.LOG)

let catch ~on fn =
  try fn ()
  with exn ->
    Log.err (fun m ->
        m "Got an unexpected exception: %S" (Printexc.to_string exn));
    on exn

exception Flow of string

module type S = sig
  type conn
  type flow

  type protect = {
    protect : 'a 'b. orphans:unit Miou.orphans -> ('a -> 'b) -> 'a -> 'b;
  }
  [@@unboxed]

  val run :
    conn ->
    ?give:Miou.Ownership.t list ->
    ?disown:(flow -> unit) ->
    read_buffer_size:int ->
    flow ->
    protect * unit Miou.t * (unit -> unit)
end

let rec terminate orphans =
  match Miou.care orphans with
  | None -> Miou.yield ()
  | Some None ->
      Miou.yield ();
      terminate orphans
  | Some (Some prm) ->
      Miou.await_exn prm;
      terminate orphans

module Make (Flow : Flow.S) (Runtime : RUNTIME) :
  S with type conn = Runtime.t and type flow = Flow.t = struct
  type conn = Runtime.t
  type flow = Flow.t

  let recv flow buffer =
    let bytes_read =
      Buffer.put buffer ~f:(fun bstr ~off:dst_off ~len ->
          let buf = Bytes.create len in
          match Flow.read flow buf ~off:0 ~len with
          | Ok 0 -> 0
          | Ok len ->
              Bigstringaf.blit_from_bytes buf ~src_off:0 bstr ~dst_off ~len;
              len
          | Error err ->
              Log.err (fun m ->
                  m "close the socket (recv) due to: %a" Flow.pp_error err);
              Flow.close flow;
              raise (Flow (Fmt.str "%a" Flow.pp_error err)))
    in
    if bytes_read = 0 then `Eof else `Ok bytes_read

  let writev flow bstrs =
    let copy { Faraday.buffer; off; len } = Bigstringaf.copy buffer ~off ~len in
    let css = List.map copy bstrs |> List.map Cstruct.of_bigarray in
    match Flow.writev flow css with
    | Ok () ->
        let len = List.fold_left (fun a { Cstruct.len; _ } -> a + len) 0 css in
        `Ok len
    | Error err ->
        Log.err (fun m ->
            m "close the socket (writev) due to: %a" Flow.pp_error err);
        Flow.close flow;
        `Closed

  type prm = Miou.Promise.Uid.t * Miou.Domain.Uid.t * int
  type _ Effect.t += Spawn : (prm:prm -> unit -> unit) -> unit Effect.t

  let pp_prm ppf (uid, runner, resources) =
    Fmt.pf ppf "[%a:%a](%d)" Miou.Domain.Uid.pp runner Miou.Promise.Uid.pp uid
      resources

  let launch ?give ?orphans fn k =
    let prm =
      Miou.call_cc ?orphans ?give @@ fun () ->
      let prm = Miou.self () in
      Log.debug (fun m -> m "%a launched" pp_prm prm);
      fn ~prm ()
    in
    let cs = Effect.Deep.get_callstack k 1_000_000 in
    Log.debug (fun m -> m "%a is launched from:" Miou.Promise.pp prm);
    Log.debug (fun m -> m "%s" (Printexc.raw_backtrace_to_string cs));
    Effect.Deep.continue k ()

  (* Protected runtime operations.

     A note on design and the need to "protect" the appearance of a new task.
     Miou constrains us on 2 points:
     1) we are obliged to observe the result of a task ([Miou.await]), otherwise
        Miou fails.
     2) only the task creator can observe the result. For example, this code
        doesn't work:

     {[
       let prm = Miou.call @@ fun () -> Miou.call (Fun.const ()) in
       Miou.await_exn (Miou.await_exn prm)
       (* the second promise (in [prm]) can only be awaited in [prm]. *)
     ]}

     The first rule requires us to use [Miou.orphans], which allows tasks to be
     stored in the background. We can then catch up with them, which is what the
     [terminate] function does. The second rule is more complicated...

     The runtime ([httpaf] or [h2]) may be able to stop a task but force the
     user to give a "continuation" allowing the same task to be restarted at the
     right time (when the user wants to do a [Body.write_string], for example).
     This is the case when [`Yield] is received from the runtime. This means
     that there are continuations (in the runtime) which can cause tasks to
     appear and which should be saved in a [Miou.orphans].

     The problem is that it's not the task that gives the continuation that will
     launch the subtask... So the best protection is to execute runtime
     functions (which may launch subtasks) with a specific [Miou.orphan].
     Continuation, on the other hand, consists in raising an effect that will be
     protected with a specific [Miou.orphans]. In other words, the subtask is
     saved not in the task [Miou.orphans] that creates the continuation, but in
     the [Miou.orphans]'s task that creates **effectfully** the subtask.

     So, after the ceremony to start an HTTP request, we need to protect the
     functions that interact with the Body as well, because they can create
     tasks too...

     {[
       let orphans, body, prm = run_http_request flow in
       Body.write_string body "Hello World";
       (* a new task appears to write ["Hello World!"] which will be saved into
          orphans. *)
       Body.close body;
       (* a new task appears to finish the HTTP request. *)
       let result = await prm in
       terminate orphans
     ]}
  *)

  let protect ?give ?orphans fn v =
    let retc = Fun.id in
    let exnc = raise in
    let open Effect.Deep in
    let effc : type c. c Effect.t -> ((c, 'b) continuation -> 'b) option =
      function
      | Spawn fn -> Some (launch ?give ?orphans fn)
      | _ -> None
    in
    match_with fn v { retc; exnc; effc }

  let next_read_operation ?give ?orphans =
    protect ?give ?orphans Runtime.next_read_operation

  let next_write_operation ?give ?orphans =
    protect ?give ?orphans Runtime.next_write_operation

  let read ?give ?orphans conn bstr ~off ~len =
    protect ?give ?orphans (Runtime.read conn ~off ~len) bstr

  let read_eof ?give ?orphans conn bstr ~off ~len =
    protect ?give ?orphans (Runtime.read_eof conn ~off ~len) bstr

  let report_exn ?give ?orphans ?(close = Fun.const ()) conn exn =
    Log.err (fun m -> m "report an exception: %S" (Printexc.to_string exn));
    protect ?give ?orphans (Runtime.report_exn conn) exn;
    Option.iter terminate orphans;
    close ()

  let report_write_result ?give ?orphans conn =
    protect ?give ?orphans (Runtime.report_write_result conn)

  let yield_reader ?give ?orphans conn =
    protect ?give ?orphans (Runtime.yield_reader conn)

  let yield_writer ?give ?orphans conn =
    protect ?give ?orphans (Runtime.yield_writer conn)

  type protect = {
    protect : 'a 'b. orphans:unit Miou.orphans -> ('a -> 'b) -> 'a -> 'b;
  }
  [@@unboxed]

  let run conn ?(give = []) ?(disown = Fun.const ()) ~read_buffer_size flow =
    let buffer = Buffer.create read_buffer_size in
    let closed = ref false in
    let close () =
      if not !closed then (
        Flow.close flow;
        closed := true)
      else disown flow
    in

    let rec reader ~prm () =
      Log.debug (fun m -> m "%a starts the reading loop" pp_prm prm);
      let rec go orphans () =
        match next_read_operation ~orphans ~give conn with
        | `Read -> (
            Log.debug (fun m -> m "%a next read operation: `read" pp_prm prm);
            let read_eof = read_eof ~orphans ~give in
            let read = read ~orphans ~give in
            match recv flow buffer with
            | `Eof ->
                Buffer.get buffer ~f:(fun bstr ~off ~len ->
                    read_eof conn bstr ~off ~len)
                |> ignore;
                go orphans ()
            | `Ok _ ->
                Buffer.get buffer ~f:(fun bstr ~off ~len ->
                    read conn bstr ~off ~len)
                |> ignore;
                go orphans ())
        | `Yield ->
            Log.debug (fun m -> m "%a next read operation: `yield" pp_prm prm);
            let continuation () =
              let prm = Miou.self () in
              Log.debug (fun m -> m "%a launches a new task" pp_prm prm);
              Effect.perform (Spawn reader)
            in
            yield_reader conn ~orphans ~give continuation;
            disown flow;
            terminate orphans
        | `Close ->
            Log.debug (fun m ->
                m "%a read: disown the file-descriptor" pp_prm prm);
            disown flow;
            terminate orphans
      in
      let orphans = Miou.orphans () in
      catch ~on:(report_exn conn ~orphans ~give ~close) @@ fun () ->
      go orphans ()
    in
    let rec writer ~prm () =
      Log.debug (fun m -> m "%a starts to the writing loop" pp_prm prm);
      let rec go orphans () =
        match next_write_operation ~orphans ~give conn with
        | `Write iovecs ->
            Log.debug (fun m -> m "%a next write operation: `write" pp_prm prm);
            writev flow iovecs |> report_write_result conn ~orphans ~give;
            go orphans ()
        | `Yield ->
            Log.debug (fun m -> m "%a next write operation: `yield" pp_prm prm);
            let continuation () =
              let prm = Miou.self () in
              Log.debug (fun m -> m "%a launches a new task" pp_prm prm);
              Effect.perform (Spawn writer)
            in
            yield_writer conn ~orphans ~give continuation;
            disown flow;
            terminate orphans
        | `Close _ ->
            Log.debug (fun m -> m "%a next write operation: `close" pp_prm prm);
            Flow.shutdown flow `Send;
            terminate orphans
      in
      let orphans = Miou.orphans () in
      catch ~on:(report_exn conn ~orphans ~give ~close) @@ fun () ->
      go orphans ()
    in
    let protect ~orphans = protect ~orphans ~give in
    let prm =
      Miou.call_cc ~give @@ fun () ->
      let p0 = Miou.call_cc ~give @@ fun () -> reader ~prm:(Miou.self ()) () in
      let p1 = Miou.call_cc ~give @@ fun () -> writer ~prm:(Miou.self ()) () in
      let result =
        match Miou.await_all [ p0; p1 ] with
        | [ Ok (); Ok () ] -> Ok ()
        | [ Error exn; _ ] | [ _; Error exn ] -> Error exn
        | _ -> assert false
      in
      Log.debug (fun m -> m "close the file-descriptor");
      (* TODO(dinosaure): we should probably check the state of the underlying
         flow and see if it's closed or not. I suspect that on [http/1.1], it's
         not the case but Miou does not complain because we [disown]
         everywhere. *)
      match result with
      | Ok () -> disown flow
      | Error exn ->
          close ();
          raise exn
    in
    Log.debug (fun m -> m "the main task is: %a" Miou.Promise.pp prm);
    ({ protect }, prm, close)
end

module TCP = struct
  type t = Miou_unix.file_descr
  type error = Unix.error * string * string

  let pp_error ppf (err, f, v) =
    Fmt.pf ppf "%s(%s): %s" f v (Unix.error_message err)

  let read flow buf ~off ~len =
    match Miou_unix.read flow buf ~off ~len with
    | len -> Ok len
    | exception Unix.Unix_error (Unix.ECONNRESET, _, _) -> Ok 0
    | exception Unix.Unix_error (err, f, v) -> Error (err, f, v)

  let full_write flow ({ Cstruct.len; _ } as cs) =
    let str = Cstruct.to_string cs in
    let rec go fd buf off len =
      if len = 0 then Ok ()
      else
        match Unix.select [] [ fd ] [] (-1.0) with
        | [], [ _ ], [] -> begin
            try
              let len' = Unix.single_write fd buf off len in
              go fd buf (off + len') (len - len')
            with
            | Unix.Unix_error (Unix.EINTR, _, _) -> go fd buf off len
            | Unix.Unix_error (err, f, v) -> Error (err, f, v)
          end
        | _ -> go fd buf off len
        | exception Unix.Unix_error (err, f, v) -> Error (err, f, v)
    in
    go (Miou_unix.to_file_descr flow) (Bytes.unsafe_of_string str) 0 len

  let writev flow css =
    let cs = Cstruct.concat css in
    full_write flow cs

  let close = Miou_unix.close

  let shutdown flow = function
    | `Recv -> Miou_unix.shutdown flow Unix.SHUTDOWN_RECEIVE
    | `Send -> Miou_unix.shutdown flow Unix.SHUTDOWN_SEND
end

module TLS = Tls_miou.Make (TCP)

type tls = TLS.t
type tls_error = TLS.error

let pp_tls_error = TLS.pp_error

let to_tls cfg ?host flow =
  match TLS.client_of_flow cfg ?host flow with
  | Ok tls_flow -> Ok tls_flow
  | Error _ as err ->
      Miou_unix.disown flow;
      err

let epoch tls =
  match tls.TLS.state with
  | `End_of_input | `Error _ -> None
  | `Active tls -> (
      match Tls.Engine.epoch tls with
      | `InitialEpoch -> assert false
      | `Epoch data -> Some data)

module Httpaf_Client_connection = struct
  include Httpaf.Client_connection

  let yield_reader _ = assert false

  let next_read_operation t =
    (next_read_operation t :> [ `Close | `Read | `Yield ])
end

(* Implementations. *)

type config = [ `V1 of Httpaf.Config.t | `V2 of H2.Config.t ]
type flow = [ `Tls of TLS.t | `Tcp of Miou_unix.file_descr ]
type request = [ `V1 of Httpaf.Request.t | `V2 of H2.Request.t ]
type response = [ `V1 of Httpaf.Response.t | `V2 of H2.Response.t ]

type 'body body = {
  body : 'body;
  write_string : 'body -> ?off:int -> ?len:int -> string -> unit;
  close : 'body -> unit;
  release : unit -> unit;
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

module TLS' = struct
  include TLS

  let close flow =
    match flow.TLS.state with
    | `Active _ -> close flow
    | _ -> Miou_unix.disown flow.TLS.flow
end

module A =
  Make
    (struct
      include TLS'

      let shutdown flow _ = Miou_unix.disown flow.flow
    end)
    (Httpaf_Client_connection)
(* XXX(dinosaure): We need to make a serious note of this. The behavior of
   http/1.1 with TLS seems rather "random" in the sense that some servers
   deliver a close-notify to the client while others do nothing... Worse still,
   if we want to deliver a close-notify to the server to say we no longer want
   to write (but still want to read), some servers close the connection and we
   get an [ECONNRESET] when we want to read (and we wouldn't have read the whole
   HTML document...).

   In other words, as far as HTTP/1.1 and TLS are concerned, we should NOT send
   a close-notify. The other subtlety is (and this is not yet the case in our
   code) to close the connection if the TLS layer fails (which is not the case
   with H2). In short, these two subtleties mean that I've spent quite a few
   days experimenting with and without the shutdown, and finally realized that
   both behaviors are necessary for HTTP/1.1 AND H2...

   However, this clarifies something quite important: thanks to Miou, we can be
   sure that no task remains in the background after the request has been sent
   and the response has been received. The interaction model with an HTTP server
   can be quite complex (long polling, h2, websocket...) and we're not immune to
   forgetting or double-clicking. Miou fails in such situations, which forced me
   to rethink the execution of an HTTP request without [Lwt.async] ;) ! *)

module B = Make (TCP) (Httpaf_Client_connection)
module C = Make (TLS') (H2.Client_connection)
module D = Make (TCP) (H2.Client_connection)

(* NOTE(dinosaure): we avoid first-class module here. *)
let run ~f acc config flow request =
  let response : response option ref = ref None
  and error = ref None
  and acc = ref acc in
  let error_handler err =
    Log.err (fun m -> m "Got an error: %a" pp_error err);
    match err with
    | `V1 (`Exn (Flow msg)) | `V2 (`Exn (Flow msg)) ->
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
      let { A.protect }, prm, close =
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
        terminate orphans;
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
      let { B.protect }, prm, close =
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
        terminate orphans;
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
      let { C.protect }, prm, close =
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
        terminate orphans;
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
      let { D.protect }, prm, close =
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
        terminate orphans;
        close ()
      in
      let write_string body ?off ?len str =
        protect ~orphans (H2.Body.Writer.write_string body ?off ?len) str
      in
      let close body = protect ~orphans H2.Body.Writer.close body in
      let body = { body; write_string; close; release } in
      Process (V2, await, body)
  | _ -> Fmt.invalid_arg "Http_miou_unix.run: incompatible arguments"
