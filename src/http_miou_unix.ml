let src = Logs.Src.create "http-miou-unix"

module Log = (val Logs.src_log src : Logs.LOG)

external reraise : exn -> 'a = "%reraise"

module TCP = struct
  type t = Miou_unix.file_descr

  let read flow ?off ?len buf =
    try Miou_unix.read flow buf ?off ?len with
    | Unix.Unix_error (Unix.ECONNRESET, _, _) -> 0
    | Unix.Unix_error _ as exn -> raise exn

  let write fd ?off ?len str =
    try Miou_unix.write fd str ?off ?len with
    | Unix.(Unix_error (EPIPE, _, _)) -> reraise Runtime.Flow.Closed_by_peer
    | exn -> reraise exn

  let close = Miou_unix.close

  let shutdown flow cmd =
    try match cmd with
      | `read -> Unix.shutdown (Miou_unix.to_file_descr flow) Unix.SHUTDOWN_RECEIVE
      | `write -> Unix.shutdown (Miou_unix.to_file_descr flow) Unix.SHUTDOWN_SEND
      | `read_write -> Unix.close (Miou_unix.to_file_descr flow)
    with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()
  [@@ocamlformat "disable"]
end

module TLS = struct
  include Tls_miou_unix

  let write fd ?off ?len str =
    try write fd ?off ?len str with
    | Tls_miou_unix.Closed_by_peer -> reraise Runtime.Flow.Closed_by_peer
    | exn -> reraise exn
end
