let src = Logs.Src.create "runtime"
let _minor = (Sys.word_size / 8 * 256) - 1

external reraise : exn -> 'a = "%reraise"

module Log = (val Logs.src_log src : Logs.LOG)

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

exception Flow of string

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
      conn: Runtime.t
    ; flow: Flow.t
    ; tasks: (unit -> unit) Queue.t
    ; buffer: Buffer.t
    ; stop: bool ref
  }

  let reader t =
    let rec go () =
      match Runtime.next_read_operation t.conn with
      | `Read ->
          let fn =
            Log.debug (fun m -> m "+reader");
            match recv t.flow t.buffer with
            | `Eof ->
                Log.debug (fun m -> m "+reader eof");
                Runtime.read_eof t.conn
            | `Ok len ->
                Log.debug (fun m -> m "+reader %d byte(s)" len);
                Runtime.read t.conn
          in
          let _ = Buffer.get t.buffer ~fn in
          go ()
      | `Yield ->
          let k () = Queue.push go t.tasks in
          Log.debug (fun m -> m "+reader yield");
          Runtime.yield_reader t.conn k
      | `Close ->
          shutdown t.flow `read;
          t.stop := true
      | `Upgrade -> Fmt.failwith "Upgrade unimplemented" (* TODO *)
    in
    go

  let writer t =
    let rec go () =
      match Runtime.next_write_operation t.conn with
      | `Write iovecs ->
          let fn acc { Faraday.len; _ } = acc + len in
          let len = List.fold_left fn 0 iovecs in
          Log.debug (fun m -> m "+write %d byte(s)" len);
          writev t.flow iovecs |> Runtime.report_write_result t.conn;
          go ()
      | `Yield ->
          let k () = Queue.push go t.tasks in
          Log.debug (fun m -> m "+writer yield");
          Runtime.yield_writer t.conn k
      | `Close _ ->
          Log.debug (fun m -> m "+writer closed");
          shutdown t.flow `write;
          t.stop := true
      | `Upgrade -> Fmt.failwith "Upgrade unimplemented" (* TODO *)
    in
    go

  let run conn ?(read_buffer_size = _minor) flow =
    let buffer = Buffer.create read_buffer_size in
    let s_reader = ref false and s_writer = ref false in
    let tasks = Queue.create () in
    let runner () =
      let rec go orphans =
        let seq = Queue.to_seq tasks in
        let lst = List.of_seq seq in
        Queue.clear tasks;
        List.iter (fun fn -> ignore (Miou.async ~orphans fn)) lst;
        match Miou.care orphans with
        | None ->
            if (not !s_reader) && not !s_writer then begin
              Miou.yield (); go orphans
            end
        | Some None -> Miou.yield (); go orphans
        | Some (Some prm) -> begin
            match Miou.await prm with
            | Ok () -> go orphans
            | Error exn ->
                Runtime.report_exn conn exn;
                go orphans
          end
      in
      go (Miou.orphans ())
    in
    let rd = reader { conn; flow; tasks; buffer; stop= s_reader } in
    let wr = writer { conn; flow; tasks; buffer; stop= s_writer } in
    Queue.push rd tasks; Queue.push wr tasks; Miou.async runner
end
