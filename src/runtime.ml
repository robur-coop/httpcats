let src = Logs.Src.create "runtime"

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
  val get : t -> f:(Bigstringaf.t -> off:int -> len:int -> int) -> int
  val put : t -> f:(Bigstringaf.t -> off:int -> len:int -> int) -> int
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

type _ Effect.t += Spawn : (unit -> unit) -> unit Effect.t

let flat_tasks fn =
  let orphans = Miou.orphans () in
  let open Effect.Deep in
  let retc value = terminate orphans; value
  and exnc exn = terminate orphans; raise exn
  and effc : type c. c Effect.t -> ((c, 'a) continuation -> 'a) option =
    function
    | Spawn fn ->
        clean orphans;
        ignore (Miou.async ~orphans fn);
        Some (fun k -> continue k ())
    | _ -> None
  in
  match_with fn orphans { retc; exnc; effc }

exception Closed_by_peer = Flow.Closed_by_peer

module Make (Flow : Flow.S) (Runtime : S) = struct
  type conn = Runtime.t
  type flow = Flow.t

  let recv flow buffer =
    let bytes_read =
      Buffer.put buffer ~f:(fun bstr ~off:dst_off ~len ->
          let buf = Bytes.create len in
          try
            let len' = Flow.read flow buf ~off:0 ~len in
            Bigstringaf.blit_from_bytes buf ~src_off:0 bstr ~dst_off ~len:len';
            len'
          with exn -> Flow.close flow; raise exn)
    in
    if bytes_read = 0 then `Eof else `Ok bytes_read

  let writev flow bstrs =
    let copy { Faraday.buffer; off; len } =
      Bigstringaf.substring buffer ~off ~len
    in
    let strs = List.map copy bstrs in
    let len = List.fold_left (fun a str -> a + String.length str) 0 strs in
    try
      List.iter (Flow.write flow) strs;
      `Ok len
    with
    | Closed_by_peer -> `Closed
    | _exn -> Flow.close flow; `Closed

  let shutdown flow cmd =
    try Flow.shutdown flow cmd
    with exn ->
      Log.err (fun m -> m "error when we shutdown: %S" (Printexc.to_string exn))

  let run conn ~read_buffer_size flow =
    let buffer = Buffer.create read_buffer_size in
    let rec reader () =
      let rec go orphans =
        match Runtime.next_read_operation conn with
        | `Read -> begin
            match recv flow buffer with
            | `Eof ->
                Log.debug (fun m -> m "the connection is closed by peer");
                let _ = Buffer.get buffer ~f:(Runtime.read_eof conn) in
                go orphans
            | `Ok len ->
                Log.debug (fun m -> m "read %d byte(s)" len);
                let _ = Buffer.get buffer ~f:(Runtime.read conn) in
                go orphans
          end
        | `Yield ->
            let k () =
              Log.debug (fun m -> m "spawn reader");
              Effect.perform (Spawn reader)
            in
            Runtime.yield_reader conn k;
            Log.debug (fun m -> m "yield the reader")
        | `Upgrade -> Fmt.failwith "Upgrade unimplemented"
        | `Close ->
            Log.debug (fun m -> m "shutdown the reader");
            shutdown flow `read
      in
      try flat_tasks go
      with exn ->
        Log.err (fun m ->
            m "report an exception the reader: %S" (Printexc.to_string exn));
        let go _orphans = Runtime.report_exn conn exn in
        flat_tasks go
    in
    let rec writer () =
      let rec go orphans =
        match Runtime.next_write_operation conn with
        | `Write iovecs ->
            let len =
              List.fold_left (fun acc { Faraday.len; _ } -> acc + len) 0 iovecs
            in
            Log.debug (fun m -> m "write %d byte(s)" len);
            writev flow iovecs |> Runtime.report_write_result conn;
            go orphans
        | `Yield ->
            let k () =
              Log.debug (fun m -> m "spawn writer");
              Effect.perform (Spawn writer)
            in
            Runtime.yield_writer conn k;
            Log.debug (fun m -> m "yield the writer")
        | `Close _ ->
            Log.debug (fun m -> m "shutdown the writer");
            shutdown flow `write
        | `Upgrade -> Fmt.failwith "Upgrade unimplemented"
      in
      try flat_tasks go
      with exn ->
        Log.err (fun m ->
            m "report an exception from the writer: %S" (Printexc.to_string exn));
        let go _orphans = Runtime.report_exn conn exn in
        flat_tasks go
    in
    Miou.async @@ fun () ->
    let p0 = Miou.async reader and p1 = Miou.async writer in
    match Miou.await_all [ p0; p1 ] with
    | [ Ok (); Ok () ] -> Log.debug (fun m -> m "reader & writer terminates")
    | [ Error exn; _ ] | [ _; Error exn ] ->
        Log.err (fun m ->
            m "got an exception from reader or writer: %S"
              (Printexc.to_string exn));
        raise exn
    | _ -> assert false
end
