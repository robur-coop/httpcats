type error

val pp_error : error Fmt.t

module Method = H2.Method
module Headers = H2.Headers
module Status = H2.Status

type stop

val stop : unit -> stop
val switch : stop -> unit

type request = {
    meth: Method.t
  ; target: string
  ; scheme: string
  ; headers: Headers.t
}

type response = { status: Status.t; headers: Headers.t }
type body = [ `V1 of [ `write ] Httpaf.Body.t | `V2 of H2.Body.Writer.t ]
type reqd = [ `V1 of Httpaf.Reqd.t | `V2 of H2.Reqd.t ]
type error_handler = ?request:request -> error -> (Headers.t -> body) -> unit
type handler = reqd -> unit

val http_1_1_server_connection :
     ?config:Httpaf.Config.t
  -> ?error_handler:error_handler
  -> handler:(Httpaf.Reqd.t -> unit)
  -> Miou_unix.file_descr
  -> unit

val clear :
     ?stop:stop
  -> ?config:Httpaf.Config.t
  -> ?backlog:int
  -> ?error_handler:error_handler
  -> handler:(Httpaf.Reqd.t -> unit)
  -> Unix.sockaddr
  -> unit

val https_1_1_server_connection :
     ?config:Httpaf.Config.t
  -> ?error_handler:error_handler
  -> handler:(Httpaf.Reqd.t -> unit)
  -> Tls_miou_unix.t
  -> unit

val h2s_server_connection :
     ?config:H2.Config.t
  -> ?error_handler:error_handler
  -> handler:(H2.Reqd.t -> unit)
  -> Tls_miou_unix.t
  -> unit

val with_tls :
     ?stop:stop
  -> ?config:
       [ `Both of Httpaf.Config.t * H2.Config.t
       | `HTTP_1_1 of Httpaf.Config.t
       | `H2 of H2.Config.t ]
  -> ?backlog:int
  -> ?error_handler:error_handler
  -> Tls.Config.server
  -> handler:handler
  -> Unix.sockaddr
  -> unit
