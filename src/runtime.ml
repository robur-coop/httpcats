let src = Logs.Src.create "runtime"
let _minor = (Sys.word_size / 8 * 256) - 1

external reraise : exn -> 'a = "%reraise"

module Log = (val Logs.src_log src : Logs.LOG)
module Flow = Flow

module type S = sig
  type t

  val next_read_operation : t -> [ `Read | `Yield | `Close | `Upgrade ]
  val read : t -> Bigstringaf.t -> off:int -> len:int -> int
  val read_eof : t -> Bigstringaf.t -> off:int -> len:int -> int
  val yield_reader : t -> (unit -> unit) -> unit

  val next_write_operation :
       t
    -> [ `Write of Bigstringaf.t Faraday.iovec list
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
  val get : t -> fn:(Bigstringaf.t -> off:int -> len:int -> int) -> int
  val put : t -> fn:(Bigstringaf.t -> off:int -> len:int -> int) -> int
end = struct
  type t = { mutable buffer: Bigstringaf.t; mutable off: int; mutable len: int }

  let create size =
    let buffer = Bigstringaf.create size in
    { buffer; off= 0; len= 0 }

  let compress t =
    if t.len = 0 then begin
      t.off <- 0;
      t.len <- 0
    end
    else if t.off > 0 then begin
      Bigstringaf.blit t.buffer ~src_off:t.off t.buffer ~dst_off:0 ~len:t.len;
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
    if Bigstringaf.length buf = t.len then begin
      t.buffer <- Bigstringaf.create (2 * Bigstringaf.length buf);
      Bigstringaf.blit buf ~src_off:t.off t.buffer ~dst_off:0 ~len:t.len
    end;
    let n = fn t.buffer ~off ~len:(Bigstringaf.length t.buffer - off) in
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
  | None -> Miou.yield ()
  | Some None -> Miou.yield ()
  | Some (Some prm) -> begin
      match Miou.await prm with
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

  let recv flow buffer =
    let bytes_read =
      Buffer.put buffer ~fn:(fun bstr ~off:dst_off ~len ->
          let len = min len _minor in
          let buf = Bytes.create len in
          try
            let len' = Flow.read flow buf ~off:0 ~len in
            Bigstringaf.blit_from_bytes buf ~src_off:0 bstr ~dst_off ~len:len';
            len'
          with exn -> Flow.close flow; reraise exn)
    in
    if bytes_read = 0 then `Eof else `Ok bytes_read

  let rec split acc bstr off len =
    if len <= _minor then List.rev (Bigstringaf.substring bstr ~off ~len :: acc)
    else
      let len' = min len _minor in
      let str = Bigstringaf.substring bstr ~off ~len:len' in
      split (str :: acc) bstr (off + len') (len - len')

  let writev flow bstrs =
    let strss =
      List.map
        (fun { Faraday.buffer; off; len } -> split [] buffer off len)
        bstrs
    in
    let len = List.fold_left (fun a { Faraday.len; _ } -> a + len) 0 bstrs in
    try
      List.iter (List.iter (Flow.write flow)) strss;
      `Ok len
    with
    | Closed_by_peer -> `Closed
    | _exn -> Flow.close flow; `Closed

  type t = {
      tags: Logs.Tag.set
    ; conn: Runtime.t
    ; flow: Flow.t
    ; tasks: (unit -> unit) Queue.t
    ; buffer: Buffer.t
    ; stop: bool ref
    ; upgrade: unit Miou.Computation.t
    ; lock: Miou.Mutex.t
    ; cond: Miou.Condition.t
  }

  let reader t =
    let rec protected () =
      match Runtime.next_read_operation t.conn with
      | `Read ->
          Log.debug (fun m -> m ~tags:t.tags "+reader");
          let fn =
            match recv t.flow t.buffer with
            | `Eof ->
                Log.debug (fun m -> m ~tags:t.tags "+reader eof");
                Runtime.read_eof t.conn
            | `Ok len ->
                Log.debug (fun m -> m ~tags:t.tags "+reader %d byte(s)" len);
                Runtime.read t.conn
          in
          let _ = Buffer.get t.buffer ~fn in
          protected ()
      | `Yield ->
          let k () =
            Miou.Mutex.protect t.lock @@ fun () ->
            Queue.push go t.tasks;
            Miou.Condition.signal t.cond
          in
          Log.debug (fun m -> m ~tags:t.tags "+reader yield");
          Runtime.yield_reader t.conn k
      | `Close ->
          shutdown t.flow `read;
          t.stop := true;
          Log.debug (fun m -> m ~tags:t.tags "+reader closed")
      | `Upgrade ->
          Log.debug (fun m -> m ~tags:t.tags "+reader upgrade");
          ignore (Miou.Computation.try_return t.upgrade ())
    and finally () =
      Log.debug (fun m -> m ~tags:t.tags "+reader signals");
      Miou.Mutex.protect t.lock @@ fun () -> Miou.Condition.signal t.cond
    and go () = Fun.protect ~finally protected in
    go

  let writer t =
    let rec protected () =
      match Runtime.next_write_operation t.conn with
      | `Write iovecs ->
          let fn acc { Faraday.len; _ } = acc + len in
          let len = List.fold_left fn 0 iovecs in
          Log.debug (fun m -> m ~tags:t.tags "+write %d byte(s)" len);
          writev t.flow iovecs |> Runtime.report_write_result t.conn;
          protected ()
      | `Yield ->
          let k () =
            Miou.Mutex.protect t.lock @@ fun () ->
            Queue.push go t.tasks;
            Miou.Condition.signal t.cond
          in
          Log.debug (fun m -> m ~tags:t.tags "+writer yield");
          Runtime.yield_writer t.conn k
      | `Close _ ->
          shutdown t.flow `write;
          t.stop := true;
          Log.debug (fun m -> m ~tags:t.tags "+writer closed")
      | `Upgrade ->
          Log.debug (fun m -> m ~tags:t.tags "+writer upgrade");
          ignore (Miou.Computation.try_return t.upgrade ())
    and finally () =
      Log.debug (fun m -> m ~tags:t.tags "+writer signals");
      Miou.Mutex.protect t.lock @@ fun () -> Miou.Condition.signal t.cond
    and go () = Fun.protect ~finally protected in
    go

  (* NOTE(dinosaure): report exception only once. *)
  let report_exn tags error conn exn =
    Log.err (fun m -> m ~tags "user's exception: %s" (Printexc.to_string exn));
    if !error = false then begin
      Runtime.report_exn conn exn;
      error := true
    end

  let rec terminate tags error conn orphans =
    match Miou.care orphans with
    | None -> ()
    | Some None ->
        Miou.yield ();
        terminate tags error conn orphans
    | Some (Some prm) -> begin
        match Miou.await prm with
        | Ok () -> terminate tags error conn orphans
        | Error exn ->
            report_exn tags error conn exn;
            terminate tags error conn orphans
      end

  let rec clean tags error conn orphans =
    match Miou.care orphans with
    | Some None | None -> Miou.yield ()
    | Some (Some prm) -> begin
        match Miou.await prm with
        | Ok () -> clean tags error conn orphans
        | Error exn ->
            report_exn tags error conn exn;
            clean tags error conn orphans
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

  let run conn ?(tags = Logs.Tag.empty) ?(read_buffer_size = _minor) ?upgrade
      flow =
    let buffer = Buffer.create read_buffer_size in
    let s_rd = ref false and s_wr = ref false and error = ref false in
    let u_rd = Miou.Computation.create () in
    let u_wr = Miou.Computation.create () in
    let tasks = Queue.create () in
    let lock = Miou.Mutex.create () in
    let cond = Miou.Condition.create () in
    let is_shutdown conn = Runtime.is_closed conn || (!s_rd && !s_wr) in
    let runner () =
      let rec go orphans =
        clean tags error conn orphans;
        let () =
          Miou.Mutex.protect lock @@ fun () ->
          if Queue.is_empty tasks && not (is_shutdown conn) then
            Miou.Condition.wait cond lock
        in
        let seq = Queue.to_seq tasks in
        let lst = List.of_seq seq in
        Queue.clear tasks;
        List.iter (fun fn -> ignore (Miou.async ~orphans fn)) lst;
        if not (is_shutdown conn) then go orphans
        else begin
          Log.debug (fun m -> m ~tags "Connection closed");
          let _ = Miou.Computation.try_cancel u_rd (Miou.Cancelled, empty_bt) in
          let _ = Miou.Computation.try_cancel u_wr (Miou.Cancelled, empty_bt) in
          ()
        end
      in
      let orphans = Miou.orphans () in
      let finally () = terminate tags error conn orphans in
      Fun.protect ~finally @@ fun () -> go orphans
    in
    let upgrade () =
      let rd = Miou.Computation.await u_rd in
      let wr = Miou.Computation.await u_wr in
      match (rd, wr, upgrade) with
      | Error _, _, _ | _, Error _, _ -> ()
      | _, _, None ->
          Log.debug (fun m -> m ~tags "No handler for websocket was given");
          Fmt.failwith "Upgrade unsupported"
      | Ok (), Ok (), Some fn ->
          let fn () =
            fn flow;
            (* TODO(upgrade)
               - multi-shutdown issue?
               - Runtime.is_closed not true after shutdown `read and `write
                 use is_shutdown instead *)
            (* need to shutdown flow here *)
            Log.debug (fun m ->
                m ~tags "Upgrade handler finished, shutdown the underlying flow");
            s_rd := true;
            shutdown flow `read;
            s_wr := true;
            shutdown flow `write;
            (* assert (Runtime.is_closed conn); *)
            assert (is_shutdown conn);
            (* notify runner so it can stop waiting *)
            Miou.Condition.signal cond
          in
          Queue.push fn tasks
    in
    let rd =
      let stop = s_rd and upgrade = u_rd in
      reader { tags; conn; flow; tasks; buffer; stop; upgrade; lock; cond }
    in
    let wr =
      let stop = s_wr and upgrade = u_wr in
      writer { tags; conn; flow; tasks; buffer; stop; upgrade; lock; cond }
    in
    Queue.push rd tasks;
    Queue.push wr tasks;
    Queue.push upgrade tasks;
    Miou.async runner
end
