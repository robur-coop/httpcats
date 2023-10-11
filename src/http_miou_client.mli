type config = [ `V1 of Httpaf.Config.t | `V2 of H2.Config.t ]
type flow = [ `Tls of Http_miou_unix.TLS.t | `Tcp of Miou_unix.file_descr ]
type request = [ `V1 of Httpaf.Request.t | `V2 of H2.Request.t ]
type response = [ `V1 of Httpaf.Response.t | `V2 of H2.Response.t ]

type error =
  [ `V1 of Httpaf.Client_connection.error
  | `V2 of H2.Client_connection.error
  | `Protocol of string ]

val pp_error : error Fmt.t

type 'body body =
  { body : 'body
  ; write_string : 'body -> ?off:int -> ?len:int -> string -> unit
  ; close : 'body -> unit
  ; release : unit -> unit
  }

type ('resp, 'body) version =
  | V1 : (Httpaf.Response.t, [ `write ] Httpaf.Body.t body) version
  | V2 : (H2.Response.t, H2.Body.Writer.t body) version

type ('resp, 'acc) await = unit -> ('resp * 'acc, error) result

type 'acc process =
  | Process :
      ('resp, 'body) version * ('resp, 'acc) await * 'body
      -> 'acc process

val run :
     f:(response -> 'acc -> string -> 'acc)
  -> 'acc
  -> config
  -> flow
  -> request
  -> 'acc process
