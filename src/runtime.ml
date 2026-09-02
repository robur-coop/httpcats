let src = Logs.Src.create "runtime"

(* NOTE(dinosaure): the initial size of the bigstring into which we accumulate
   what the peer sends us (it grows on demand, see [Buffer.put]). Almost every
   caller overrides it with [H1.Config.read_buffer_size] /
   [H2.Config.read_buffer_size] (0x1000 for [h1]); the default only applies
   where no such configuration exists (the websocket connections).

   Note that the effective read is [min read_buffer_size (free space)], so it
   is really the [h1]/[h2] configuration which decides how much we read at a
   time. Measured on a 64 MiB upload over loopback: 0x1000 gives 2.2 GB/s,
   0x4000 gives 3.6 GB/s and it plateaus from there. *)
let default_read_buffer_size = 0x4000

module Log = (val Logs.src_log src : Logs.LOG)
module Flow = Flow

module type S = sig
  type t

  val next_read_operation : t -> [ `Read | `Yield | `Close | `Upgrade ]
  val read : t -> Bstr.t -> off:int -> len:int -> int
  val read_eof : t -> Bstr.t -> off:int -> len:int -> int
  val yield_reader : t -> (unit -> unit) -> unit

  val next_write_operation :
       t
    -> [ `Write of Bstr.t Faraday.iovec list
       | `Close of int
       | `Yield
       | `Upgrade ]

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  val yield_writer : t -> (unit -> unit) -> unit
  val report_exn : t -> exn -> unit
  val is_closed : t -> bool
end

module Buffer : sig
  type t

  val create : int -> t
  val get : t -> fn:(Bstr.t -> off:int -> len:int -> int) -> int
  val put : t -> fn:(Bstr.t -> off:int -> len:int -> int) -> int
end = struct
  type t = { mutable buffer: Bstr.t; mutable off: int; mutable len: int }

  let create size =
    let buffer = Bstr.create size in
    { buffer; off= 0; len= 0 }

  let compress t =
    if t.len = 0 then begin
      t.off <- 0;
      t.len <- 0
    end
    else if t.off > 0 then begin
      Bstr.blit t.buffer ~src_off:t.off t.buffer ~dst_off:0 ~len:t.len;
      t.off <- 0
    end

  let get t ~fn =
    let n = fn t.buffer ~off:t.off ~len:t.len in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0 then t.off <- 0;
    n

  let put t ~fn =
    compress t;
    let off = t.off + t.len in
    let buf = t.buffer in
    if Bstr.length buf = t.len then begin
      t.buffer <- Bstr.create (2 * Bstr.length buf);
      Bstr.blit buf ~src_off:t.off t.buffer ~dst_off:0 ~len:t.len
    end;
    let n = fn t.buffer ~off ~len:(Bstr.length t.buffer - off) in
    t.len <- t.len + n;
    n
end

let empty_bt = Printexc.get_callstack max_int

let rec terminate orphans =
  match Miou.care orphans with
  | None -> Miou.yield ()
  | Some None -> Miou.yield (); terminate orphans
  | Some (Some prm) -> (
      match Miou.await prm with
      | Ok () -> terminate orphans
      | Error exn ->
          Log.err (fun m ->
              m "unexpected exception from an asynchronous task: %S"
                (Printexc.to_string exn));
          terminate orphans)

let rec clean orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) ->
      begin match Miou.await prm with
      | Ok () -> clean orphans
      | Error exn ->
          Log.err (fun m ->
              m "unexpected exception from an asynchronous task: %S"
                (Printexc.to_string exn));
          clean orphans
      end

exception Closed_by_peer = Flow.Closed_by_peer

module type CONNECTION = sig
  type conn
  type flow

  val run :
       conn
    -> ?tags:Logs.Tag.set
    -> ?read_buffer_size:int
    -> ?upgrade:(flow -> unit)
    -> flow
    -> unit Miou.t
end

module Make (Flow : Flow.S) (Runtime : S) = struct
  type conn = Runtime.t
  type flow = Flow.t

  let shutdown flow cmd =
    try Flow.shutdown flow cmd
    with exn ->
      Log.err (fun m -> m "error when we shutdown: %S" (Printexc.to_string exn))
  (* TODO(dinosaure): It can happen that we try to shutdown a connection when it
     is already closed (it all depends on the behavior of the peer). It seems
     that the closing of a socket between two peers via HTTP is not as
     standardized as all that. Thus, shutdown can raise an exception (saying
     that the connection has already been closed by the peer).

     We could check before attempting to shutdown the connection instead of
     ignoring the exception that may have been raised. *)

  let recv flow buffer =
    let bytes_read = Buffer.put buffer ~fn:(Flow.read flow) in
    if bytes_read = 0 then `Eof else `Ok bytes_read

  let writev flow bstrs =
    let len = List.fold_left (fun a { Faraday.len; _ } -> a + len) 0 bstrs in
    try Flow.writev flow bstrs; `Ok len with
    | Closed_by_peer -> `Closed
    | _exn -> `Closed

  type t = {
      tags: Logs.Tag.set
    ; conn: Runtime.t
    ; flow: Flow.t
    ; buffer: Buffer.t
    ; stop: bool ref
    ; upgrade: unit Miou.Computation.t
  }

  let yield ~name:_ t register =
    let waker = Miou.Computation.create () in
    register t.conn (fun () -> ignore (Miou.Computation.try_return waker ()));
    match Miou.Computation.await waker with
    | Ok () -> `Continue
    | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt

  let reader t =
    let rec protected () =
      match Runtime.next_read_operation t.conn with
      | `Read ->
          let fn =
            Log.debug (fun m -> m "+read reader");
            match recv t.flow t.buffer with
            | `Eof ->
                Log.debug (fun m -> m "the flow was closed by peer");
                Runtime.read_eof t.conn
            | `Ok len ->
                Log.debug (fun m -> m "got %d byte(s) from the given flow" len);
                Runtime.read t.conn
          in
          let _ = Buffer.get t.buffer ~fn in
          protected ()
      | `Yield ->
          let `Continue = yield ~name:"reader" t Runtime.yield_reader in
          protected ()
      | `Close ->
          Log.debug (fun m -> m "+close reader");
          shutdown t.flow `read;
          t.stop := true
      | `Upgrade -> ignore (Miou.Computation.try_return t.upgrade ())
    in
    protected

  let writer t =
    let rec protected () =
      match Runtime.next_write_operation t.conn with
      | `Write iovecs ->
          Log.debug (fun m -> m "+write writer");
          writev t.flow iovecs |> Runtime.report_write_result t.conn;
          protected ()
      | `Yield ->
          let `Continue = yield ~name:"writer" t Runtime.yield_writer in
          protected ()
      | `Close _ ->
          Log.debug (fun m -> m "+close writer");
          shutdown t.flow `write;
          t.stop := true
      | `Upgrade -> ignore (Miou.Computation.try_return t.upgrade ())
    in
    protected

  type g = {
      tags: Logs.Tag.set
    ; conn: Runtime.t
    ; flow: Flow.t
    ; buffer: Buffer.t
    ; rd_stop: bool ref
    ; wr_stop: bool ref
    ; errored: bool ref
    ; rd_resolver: unit Miou.Computation.t
    ; wr_resolver: unit Miou.Computation.t
  }

  (* NOTE(dinosaure): report exception only once. *)
  let report_exn g exn =
    Log.err (fun m ->
        m ~tags:g.tags "user's exception: %s" (Printexc.to_string exn));
    if !(g.errored) = false then begin
      Runtime.report_exn g.conn exn;
      g.errored := true
    end

  let guarded g fn () =
    try fn ()
    with exn ->
      report_exn g exn;
      g.rd_stop := true;
      g.wr_stop := true;
      shutdown g.flow `read_write

  (* NOTE(dinosaure): a connection is three tasks under one "runner": a reader,
     a writer, and one waiting for a possible protocol upgrade. The runner
     creates them and awaits them.

     NOTE(dinosaure): [Runtime.is_closed] does not mean that there are no more
     tasks and that the connection can be "terminated" (via [Miou.await_exn] or
     [Miou.cancel]); it merely indicates that our internal state is closed. The
     only way to know whether we should indeed terminate the tasks is to trust
     the [Runtime] state machine and expect that [`Close] is indeed issued by
     the writer and the reader. It should be noted that an exception may be
     thrown by the network layer, and this must be reported by the state machine
     (via [report_exn]) to signal our main loop to stop everything (particularly
     when a client connection is interrupted by a [^C]). *)

  let to_reader g =
    {
      tags= g.tags
    ; conn= g.conn
    ; flow= g.flow
    ; buffer= g.buffer
    ; stop= g.rd_stop
    ; upgrade= g.rd_resolver
    }

  let to_writer g =
    {
      tags= g.tags
    ; conn= g.conn
    ; flow= g.flow
    ; buffer= g.buffer
    ; stop= g.wr_stop
    ; upgrade= g.wr_resolver
    }

  let global ~read_buffer_size ~tags conn flow =
    let buffer = Buffer.create read_buffer_size in
    let rd_stop = ref false in
    let wr_stop = ref false in
    let errored = ref false in
    let rd_resolver = Miou.Computation.create () in
    let wr_resolver = Miou.Computation.create () in
    {
      tags
    ; conn
    ; flow
    ; buffer
    ; rd_stop
    ; wr_stop
    ; errored
    ; rd_resolver
    ; wr_resolver
    }

  let cancel = (Miou.Cancelled, empty_bt)

  let run conn ?(tags = Logs.Tag.empty)
      ?(read_buffer_size = default_read_buffer_size) ?upgrade flow =
    let g = global ~read_buffer_size ~tags conn flow in
    let upgrade_task () =
      let rd = Miou.Computation.await g.rd_resolver in
      let wr = Miou.Computation.await g.wr_resolver in
      match (rd, wr, upgrade) with
      | Error _, _, _ | _, Error _, _ -> ()
      | _, _, None ->
          Log.debug (fun m -> m ~tags "No handler for websocket was given");
          Fmt.failwith "Upgrade unsupported"
      | Ok (), Ok (), Some fn ->
          fn flow;
          Log.debug (fun m ->
              m ~tags "Upgrade handler finished, shutdown the underlying flow");
          shutdown flow `read;
          shutdown flow `write;
          g.rd_stop := true;
          g.wr_stop := true
    in
    let runner () =
      let prm_rd = Miou.async (guarded g (reader (to_reader g))) in
      let prm_wr = Miou.async (guarded g (writer (to_writer g))) in
      let prm_up = Miou.async (guarded g upgrade_task) in
      let _ = Miou.await_all [ prm_rd; prm_wr ] in
      let _ = Miou.Computation.try_cancel g.rd_resolver cancel in
      let _ = Miou.Computation.try_cancel g.wr_resolver cancel in
      let _ = Miou.await prm_up in
      Log.debug (fun m -> m ~tags "Connection closed")
    in
    Miou.async runner
end
