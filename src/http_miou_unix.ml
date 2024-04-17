let src = Logs.Src.create "http-miou-unix"

module Log = (val Logs.src_log src : Logs.LOG)

module TCP = struct
  type t = Miou_unix.file_descr
  type error = [ `Unix of Unix.error * string * string | `Closed ]

  let pp_error ppf = function
    | `Unix (err, f, v) -> Fmt.pf ppf "%s(%s): %s" f v (Unix.error_message err)
    | `Closed -> Fmt.string ppf "Connection closed by peer (tcp)"

  let read flow buf ~off ~len =
    match Miou_unix.read flow buf off len with
    | len -> Ok len
    | exception Unix.Unix_error (Unix.ECONNRESET, _, _) -> Ok 0
    | exception Unix.Unix_error (err, f, v) -> Error (`Unix (err, f, v))

  let full_write flow ({ Cstruct.len; _ } as cs) =
    let str = Cstruct.to_string cs in
    Miou_unix.write flow str 0 len

  let writev flow css =
    let cs = Cstruct.concat css in
    full_write flow cs; Ok ()

  let close = Miou_unix.close

  let shutdown flow cmd =
    try match cmd with
      | `read -> Unix.shutdown (Miou_unix.to_file_descr flow) Unix.SHUTDOWN_RECEIVE
      | `write -> Unix.shutdown (Miou_unix.to_file_descr flow) Unix.SHUTDOWN_SEND
      | `read_write -> Unix.close (Miou_unix.to_file_descr  flow)
    with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()
  [@@ocamlformat "disable"]
end

module TLS = Tls_miou.Make (TCP)

let to_tls cfg ?host flow = TLS.client_of_flow cfg ?host flow

let epoch tls =
  match tls.TLS.state with
  | `Active tls ->
      ( match Tls.Engine.epoch tls with
      | Error () -> assert false
      | Ok data -> Some data )
  | _ -> None
[@@ocamlformat "disable"]
