let src = Logs.Src.create "runtime"
let _minor = 16384 - 1

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

  let recv flow read_buf buffer =
    let bytes_read =
      Buffer.put buffer ~fn:(fun bstr ~off:dst_off ~len ->
          let len = min len _minor in
          let len' = Flow.read flow read_buf ~off:0 ~len in
          Bstr.blit_from_bytes read_buf ~src_off:0 bstr ~dst_off ~len:len';
          len')
    in
    if bytes_read = 0 then `Eof else `Ok bytes_read

  let writev flow write_buf bstrs =
    let len = List.fold_left (fun a { Faraday.len; _ } -> a + len) 0 bstrs in
    let fn { Faraday.buffer; off; len } =
      let rec go src_off len =
        if len > 0 then begin
          let n = Int.min len _minor in
          Bstr.blit_to_bytes buffer ~src_off write_buf ~dst_off:0 ~len:n;
          Flow.write flow ~off:0 ~len:n (Bytes.unsafe_to_string write_buf);
          go (src_off + n) (len - n)
        end
      in
      go off len
    in
    try List.iter fn bstrs; `Ok len with
    | Closed_by_peer -> `Closed
    | _exn -> `Closed

  type t = {
      tags: Logs.Tag.set
    ; conn: Runtime.t
    ; flow: Flow.t
    ; tasks: (unit -> unit) Queue.t
    ; buffer: Buffer.t
    ; tmp: bytes
    ; stop: bool ref
    ; upgrade: unit Miou.Computation.t
    ; lock: Miou.Mutex.t
    ; cond: Miou.Condition.t
  }

  let reader t =
    let rec protected () =
      match Runtime.next_read_operation t.conn with
      | `Read ->
          let fn =
            Log.debug (fun m -> m "+read reader");
            match recv t.flow t.tmp t.buffer with
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
          Log.debug (fun m -> m "+yield reader");
          let k () =
            Miou.Mutex.protect t.lock @@ fun () ->
            Queue.push go t.tasks;
            Miou.Condition.signal t.cond
          in
          Runtime.yield_reader t.conn k
      | `Close ->
          Log.debug (fun m -> m "+close reader");
          shutdown t.flow `read;
          t.stop := true
      | `Upgrade -> ignore (Miou.Computation.try_return t.upgrade ())
    and finally () =
      Miou.Mutex.protect t.lock @@ fun () -> Miou.Condition.signal t.cond
    and go () = Fun.protect ~finally protected in
    go

  let writer t =
    let rec protected () =
      match Runtime.next_write_operation t.conn with
      | `Write iovecs ->
          Log.debug (fun m -> m "+write writer");
          writev t.flow t.tmp iovecs |> Runtime.report_write_result t.conn;
          protected ()
      | `Yield ->
          Log.debug (fun m -> m "+yield writer");
          let k () =
            Miou.Mutex.protect t.lock @@ fun () ->
            Queue.push go t.tasks;
            Miou.Condition.signal t.cond
          in
          Runtime.yield_writer t.conn k
      | `Close _ ->
          Log.debug (fun m -> m "+close writer");
          shutdown t.flow `write;
          t.stop := true
      | `Upgrade -> ignore (Miou.Computation.try_return t.upgrade ())
    and finally () =
      Miou.Mutex.protect t.lock @@ fun () -> Miou.Condition.signal t.cond
    and go () = Fun.protect ~finally protected in
    go

  type g = {
      tags: Logs.Tag.set
    ; conn: Runtime.t
    ; flow: Flow.t
    ; tasks: (unit -> unit) Queue.t
    ; buffer: Buffer.t
    ; rd_buf: bytes
    ; wr_buf: bytes
    ; rd_stop: bool ref
    ; wr_stop: bool ref
    ; errored: bool ref
    ; rd_resolver: unit Miou.Computation.t
    ; wr_resolver: unit Miou.Computation.t
    ; lock: Miou.Mutex.t
    ; cond: Miou.Condition.t
  }

  (* NOTE(dinosaure): report exception only once. *)
  let report_exn g exn =
    Log.err (fun m ->
        m ~tags:g.tags "user's exception: %s" (Printexc.to_string exn));
    if !(g.errored) = false then begin
      Runtime.report_exn g.conn exn;
      g.errored := true
    end

  let drain orphans =
    let seq = Seq.of_dispenser @@ fun () -> Miou.take orphans in
    let lst = List.of_seq seq in
    List.iter Miou.cancel lst

  let rec clean g orphans =
    match Miou.care orphans with
    | Some None | None -> ()
    | Some (Some prm) ->
        begin match Miou.await prm with
        | Ok () -> clean g orphans
        | Error exn -> report_exn g exn; clean g orphans
        end

  (* NOTE(dinosaure): [Runtime] design is a "runner" process that is awaiting
     tasks. At the very beginning, we launch 2 tasks (one for reading and one
     for writing). These can involve the creation of new tasks (via [`Yield]).
     To respect the rule of relationship between tasks, the creation of these
     is not done directly via [Miou.async] but transmitted to our "runner"
     process via a queue.

     It is then our runner which will really create these tasks (and probably
     clean up the previous ones). To prevent "runner" from being a hot-loop, a
     mutex and a condition are used so that the process is waiting for a change
     of state (the addition of a new task or a change of state of [conn] after
     one of the tasks has finished).

     We trust [Runtime.is_closed] to complete our process, but it seems that it
     cannot be fully trusted. There are [s_rd] and [s_wr] which
     determine the status of the socket (whether it is closed for reading and/or
     writing). These are not currently used but may be complementary in
     determining the shutdown of [runner]. *)

  let to_reader g =
    {
      tags= g.tags
    ; conn= g.conn
    ; flow= g.flow
    ; tasks= g.tasks
    ; buffer= g.buffer
    ; tmp= g.rd_buf
    ; stop= g.rd_stop
    ; upgrade= g.rd_resolver
    ; lock= g.lock
    ; cond= g.cond
    }

  let to_writer g =
    {
      tags= g.tags
    ; conn= g.conn
    ; flow= g.flow
    ; tasks= g.tasks
    ; buffer= g.buffer
    ; tmp= g.wr_buf
    ; stop= g.wr_stop
    ; upgrade= g.wr_resolver
    ; lock= g.lock
    ; cond= g.cond
    }

  let global ~read_buffer_size ~tags conn flow =
    let buffer = Buffer.create read_buffer_size in
    let rd_buf = Bytes.create _minor in
    let wr_buf = Bytes.create _minor in
    let rd_stop = ref false in
    let wr_stop = ref false in
    let errored = ref false in
    let rd_resolver = Miou.Computation.create () in
    let wr_resolver = Miou.Computation.create () in
    let tasks = Queue.create () in
    let lock = Miou.Mutex.create () in
    let cond = Miou.Condition.create () in
    {
      tags
    ; conn
    ; flow
    ; tasks
    ; buffer
    ; rd_buf
    ; wr_buf
    ; rd_stop
    ; wr_stop
    ; errored
    ; rd_resolver
    ; wr_resolver
    ; lock
    ; cond
    }

  let run conn ?(tags = Logs.Tag.empty) ?(read_buffer_size = _minor) ?upgrade
      flow =
    let g = global ~read_buffer_size ~tags conn flow in
    let is_shutdown conn =
      Runtime.is_closed conn || (!(g.rd_stop) && !(g.wr_stop))
    in
    let runner () =
      let rec go orphans =
        clean g orphans;
        let () =
          Miou.Mutex.protect g.lock @@ fun () ->
          if Queue.is_empty g.tasks && not (is_shutdown g.conn) then
            Miou.Condition.wait g.cond g.lock
        in
        let seq = Queue.to_seq g.tasks in
        let lst = List.of_seq seq in
        Queue.clear g.tasks;
        Log.debug (fun m -> m "+%d task(s)" (List.length lst));
        List.iter (fun fn -> ignore (Miou.async ~orphans fn)) lst;
        if not (is_shutdown g.conn) then go orphans
        else begin
          Log.debug (fun m -> m ~tags "Connection closed");
          let _ =
            Miou.Computation.try_cancel g.rd_resolver (Miou.Cancelled, empty_bt)
          in
          let _ =
            Miou.Computation.try_cancel g.wr_resolver (Miou.Cancelled, empty_bt)
          in
          (* If one of the half-close steps did not happen on its own ([rd_stop]
             or [wr_stop] still false), ask the [Flow] for a terminal shutdown.
             On Linux a kernel-level shutdown turns any in-flight [read(2)]
             readable via [POLLHUP] - the [Miou_unix] poller wakes the
             reader task, [read] returns 0, and the reader unwinds normally
             via [read_eof] + [`Close]. This is what breaks the deadlock
             when a state-machine decides to close from outside the reader
             task (e.g. [shutdown_reader] called from the H2 [Writer.flush]
             callback, or from [report_exn]).

             The [Flow] implementation may legitimately turn this into a
             no-op (cf. [TCP_and_H1] for HTTP/1.1 server, where every
             [shutdown_reader] path is reached from inside the reader task
             itself, so the deadlock does not occur, and where closing the
             fd here would race the writer task that has been queued via
             [wakeup_writer] but not yet drained, truncating the response
             and producing EBADF on the next [shutdown `write]). *)
          if (not !(g.rd_stop)) || not !(g.wr_stop) then
            shutdown flow `read_write
        end
      in
      let orphans = Miou.orphans () in
      let finally () = drain orphans in
      Fun.protect ~finally @@ fun () ->
      go orphans;
      Log.debug (fun m -> m "Runtime terminated, drain tasks")
    in
    let upgrade () =
      let rd = Miou.Computation.await g.rd_resolver in
      let wr = Miou.Computation.await g.wr_resolver in
      match (rd, wr, upgrade) with
      | Error _, _, _ | _, Error _, _ -> ()
      | _, _, None ->
          Log.debug (fun m -> m ~tags "No handler for websocket was given");
          Fmt.failwith "Upgrade unsupported"
      | Ok (), Ok (), Some fn ->
          let fn () =
            fn flow;
            Log.debug (fun m ->
                m ~tags "Upgrade handler finished, shutdown the underlying flow");
            shutdown flow `read;
            shutdown flow `write;
            g.rd_stop := true;
            g.wr_stop := true;
            assert (is_shutdown g.conn);
            Miou.Condition.signal g.cond
          in
          Queue.push fn g.tasks
    in
    let rd = reader (to_reader g) in
    let wr = writer (to_writer g) in
    Queue.push rd g.tasks;
    Queue.push wr g.tasks;
    Queue.push upgrade g.tasks;
    Miou.async runner
end
