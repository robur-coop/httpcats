let src = Logs.Src.create "http-miou-unix"

module Log = (val Logs.src_log src : Logs.LOG)

external reraise : exn -> 'a = "%reraise"

(* NOTE(dinosaure): [Unix.read_bigarray] and [Unix.single_write_bigarray] exist
   since OCaml 5.02 and let us read into (and write from) the connection's
   bigstring without any intermediate [bytes]/[string]: the kernel writes
   exactly where [h1]/[h2] parse from, and [Faraday]'s iovecs go to [write(2)]
   as they are (no slicing either).

   [Miou_unix] does not expose these, but it does expose {!Miou_unix.blocking_read}
   and {!Miou_unix.blocking_write}, which is all we need to reimplement its
   [EAGAIN] loop here. This assumes the file descriptor is in non-blocking
   mode, which is what {!Miou_unix.accept}, {!Miou_unix.tcpv4} and
   {!Miou_unix.tcpv6} always give us. *)
module TCP = struct
  type t = Miou_unix.file_descr

  let read fd bstr ~off ~len =
    match Miou_unix.read_bigstring fd ~off ~len bstr with
    | exception Unix.(Unix_error (ECONNRESET, _, _)) -> 0
    | len -> len

  let writev fd bstrs =
    let fn { Faraday.buffer; off; len } =
      match Miou_unix.write_bigstring fd buffer ~off ~len with
      | exception Unix.(Unix_error (EPIPE, _, _)) ->
          reraise Runtime.Flow.Closed_by_peer
      | () -> ()
    in
    List.iter fn bstrs

  let close = Miou_unix.close

  let shutdown flow cmd =
    try match cmd with
      | `read -> Unix.shutdown (Miou_unix.to_file_descr flow) Unix.SHUTDOWN_RECEIVE
      | `write -> Unix.shutdown (Miou_unix.to_file_descr flow) Unix.SHUTDOWN_SEND
      | `read_write -> Unix.close (Miou_unix.to_file_descr flow)
    with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()
  [@@ocamlformat "disable"]
end

(* NOTE(dinosaure): [ocaml-tls] decrypts into a [string], so a TLS flow cannot
   avoid one copy into the connection's bigstring. {!Runtime.Flow.Of_bytes}
   owns the scratch buffer that copy needs - it used to be allocated by the
   [Runtime] for every flow, TLS or not. *)
module TLS = Runtime.Flow.Of_bytes (struct
  include Tls_miou_unix

  let write fd ?off ?len str =
    try write fd ?off ?len str with
    | Tls_miou_unix.Closed_by_peer -> reraise Runtime.Flow.Closed_by_peer
    | exn -> reraise exn
end)
