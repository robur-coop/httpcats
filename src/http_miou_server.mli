type error
(** The type of errors. *)

val pp_error : error Fmt.t
(** Pretty-printer of {!error}s. *)

module Method = H2.Method
(** Request methods. *)

module Headers = H2.Headers
(** Header fields.

    Case-insensitive key-value pairs. *)

module Status = H2.Status
(** Response Status codes.

    A three-digit integer, the result of the request. *)

type stop
(** Type of switches to stop a HTTP server. *)

val stop : unit -> stop
(** [create ()] creates a new switch to stop a HTTP server. *)

val switch : stop -> unit
(** [switch stop] turns off the HTTP server associated to the given [stop]. It
    call registered (by the HTTP server) finalizers and terminates. If the given
    switch is already off, it does nothing. *)

type flow = [ `Tls of Tls_miou_unix.t | `Tcp of Miou_unix.file_descr ]
(** The type of connection used to communicate with the client — whether the
    connection is secure ([`Tls]) or not ([`Tcp]). *)

type request = {
    meth: Method.t
  ; target: string
  ; scheme: string
  ; headers: Headers.t
}
(** A request consisting of a method (see {!module:Method}), a {i target} (the
    path requested by the client), a scheme (whether the client used ["http"] or
    ["https"]) and a headers. *)

type response = { status: Status.t; headers: Headers.t }
(** A response, consisting of status and headers. *)

type body = [ `V1 of H1.Body.Writer.t | `V2 of H2.Body.Writer.t ]
type reqd = [ `V1 of H1.Reqd.t | `V2 of H2.Reqd.t ]

type error_handler =
  [ `V1 | `V2 ] -> ?request:request -> error -> (Headers.t -> body) -> unit

type handler = flow -> reqd -> unit

val clear :
     ?parallel:bool
  -> ?stop:stop
  -> ?config:H1.Config.t
  -> ?backlog:int
  -> ?ready:unit Miou.Computation.t
  -> ?error_handler:error_handler
  -> ?upgrade:(Miou_unix.file_descr -> unit)
  -> handler:handler
  -> Unix.sockaddr
  -> unit

val with_tls :
     ?parallel:bool
  -> ?stop:stop
  -> ?config:
       [ `Both of H1.Config.t * H2.Config.t
       | `HTTP_1_1 of H1.Config.t
       | `H2 of H2.Config.t ]
  -> ?backlog:int
  -> ?ready:unit Miou.Computation.t
  -> ?error_handler:error_handler
  -> Tls.Config.server
  -> ?upgrade:(Tls_miou_unix.t -> unit)
  -> handler:handler
  -> Unix.sockaddr
  -> unit

module Websocket : sig
  type elt =
    [ `Connection_close
    | `Msg of H1.Websocket.Opcode.standard_non_control * bool
    | `Other
    | `Ping
    | `Pong ]
    * string

  type ic = unit -> elt option
  type oc = elt -> unit
  type stop

  val stop : unit -> stop
  val switch : stop -> unit
  val upgrade : ?stop:stop -> fn:(ic -> oc -> unit) -> flow -> unit
end
