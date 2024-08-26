type config = [ `V1 of H1.Config.t | `V2 of H2.Config.t ]
type flow = [ `Tls of Tls_miou_unix.t | `Tcp of Miou_unix.file_descr ]
type request = [ `V1 of H1.Request.t | `V2 of H2.Request.t ]
type response = [ `V1 of H1.Response.t | `V2 of H2.Response.t ]

type error =
  [ `V1 of H1.Client_connection.error
  | `V2 of H2.Client_connection.error
  | `Protocol of string ]

val pp_error : error Fmt.t

type ('resp, 'body) version =
  | V1 : (H1.Response.t, H1.Body.Writer.t) version
  | V2 : (H2.Response.t, H2.Body.Writer.t) version

type 'resp await = unit -> ('resp, error) result

type 'acc process =
  | Process :
      ('resp, 'body) version * ('resp * 'acc) await * 'body
      -> 'acc process

val run :
     f:(response -> 'acc -> string -> 'acc)
  -> 'acc
  -> config
  -> flow
  -> request
  -> 'acc process
