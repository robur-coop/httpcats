let src = Logs.Src.create "http-miou-unix"

module Log = (val Logs.src_log src : Logs.LOG)

module TCP = struct
  type t = Miou_unix.file_descr

  let read flow ?off ?len buf =
    try Miou_unix.read flow buf ?off ?len with
    | Unix.Unix_error (Unix.ECONNRESET, _, _) -> 0
    | Unix.Unix_error _ as exn -> raise exn

  let write = Miou_unix.write
  let close = Miou_unix.close

  let shutdown flow cmd =
    try match cmd with
      | `read -> Unix.shutdown (Miou_unix.to_file_descr flow) Unix.SHUTDOWN_RECEIVE
      | `write -> Unix.shutdown (Miou_unix.to_file_descr flow) Unix.SHUTDOWN_SEND
      | `read_write -> Unix.close (Miou_unix.to_file_descr flow)
    with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()
  [@@ocamlformat "disable"]
end
