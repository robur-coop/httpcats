type tls
type tls_error

val pp_tls_error : tls_error Fmt.t

val to_tls :
  Tls.Config.client ->
  ?host:[ `host ] Domain_name.t ->
  Miou_unix.file_descr ->
  (tls, tls_error) result

val epoch : tls -> Tls.Core.epoch_data option

type config = [ `V1 of Httpaf.Config.t | `V2 of H2.Config.t ]
type flow = [ `Tls of tls | `Tcp of Miou_unix.file_descr ]
type request = [ `V1 of Httpaf.Request.t | `V2 of H2.Request.t ]

type error =
  [ `V1 of Httpaf.Client_connection.error
  | `V2 of H2.Client_connection.error
  | `Protocol of string ]

val pp_error : error Fmt.t

type 'body body = {
  body : 'body;
  write_string : 'body -> ?off:int -> ?len:int -> string -> unit;
  close : 'body -> unit;
}

type ('resp, 'body) version =
  | V1 : (Httpaf.Response.t, [ `write ] Httpaf.Body.t body) version
  | V2 : (H2.Response.t, H2.Body.Writer.t body) version

type ('resp, 'acc) await = unit -> ('resp * 'acc, error) result

type 'acc process =
  | Process :
      ('resp, 'body) version * ('resp, 'acc) await * 'body
      -> 'acc process

val terminate : unit Miou.orphans -> unit

val run :
  f:('acc -> string -> 'acc) ->
  'acc ->
  config ->
  flow ->
  request ->
  unit Miou.orphans * 'acc process
