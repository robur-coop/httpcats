let src = Logs.Src.create "http-miou-unix"

module Log = (val Logs.src_log src : Logs.LOG)

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

module TLS = struct
  include Tls_miou.Make (TCP)

  let close flow =
    match flow.state with
    | `Active _ -> close flow
    | _ -> Miou_unix.disown flow.flow
end

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

(* Implementations. *)

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
