(** [Server] rovides an API for users to experiment with creating an HTTP
    server. We strongly recommend that readers use [Vif] instead of this module
    if they wish to obtain an HTTP server. *)

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
(** [stop ()] creates a new switch to stop a HTTP server. *)

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

(** Initialising an HTTP server mainly requires specifying the request handler.
    [httpcats] offers other options, which are described below.

    {2:config Server configuration}

    It is possible to configure the underlying implementation used to process
    the HTTP protocol. There are two configurations: [H1.Config.t] and
    [H2.Config.t]. The second is {b only} available if you want to initialise a
    server with TLS (see {!val:with_tls}. These settings allow you to specify
    the size of the internal buffers for processing HTTP requests and issuing
    HTTP responses.

    {2:stop Start & stop the server}

    When the server is initialised, a background process is launched to process
    HTTP requests. This process {b must} be shut down according to Miou rules
    (tasks cannot be forgotten). The {!type:stop} value indicates that you want
    to shut down the server.

    Here is an example of how to shut down the server if you receive the
    [SIGINT] signal ([^C]):

    {[
      let () =
        Miou_unix.run @@ fun () ->
        let stop = Httpcats.Server.stop () in
        let fn _sigint = Httpcats.Server.switch stop in
        ignore (Miou.sys_signal Sys.sigint (Sys.Signal_handle fn));
        Httpcats.Server.clear ~stop ~handler sockaddr
    ]}

    {2:ready Ready state of the server}

    Initialising the server means that a {i socket} is in [LISTENING] mode so
    that clients can communicate with it. The [ready] value ensures that, after
    waiting for the result, the server is actually ready to handle requests.

    Here is an example where a server is initialised and [httpcats] is then used
    to communicate with it:

    {[
      let () = Miou_unix.run @@ fun () ->
        let ready = Miou.Computation.create () in
        let prm = Miou.async @@ fun () -> Httpcats.Server.clear ~ready ... in
        Miou.Computation.await_exn ready;
        Httpcats.request ~uri:"http://localhost:8080/" ~fn ...
    ]}

    {2:parallel Parallel server.}

    By default (and by design), an [httpcats] server runs in parallel. This
    means that as soon as a client connects to the server, the {!type:handler}
    for managing the client's request will be dispatched to a domain and will
    therefore run in parallel with other clients and in parallel with the
    process accepting incoming connections.

    Parallelism is only available if you have {b at least} 2 domains available
    and if the [~parallel] option is not set to [false].

    There may be another strategy for parallelism, which consists of creating as
    many servers as there are domains available:

    {[
      let () = Miou_unix.run @@ fun () ->
        let fn _ = Httpcats.Server.clear ~parallel:false ... in
        let domains = Miou.Domains.available () in
        Miou.parallel fn (List.init domains Fun.id)
        |> List.iter (function Ok v -> v | Error exn -> raise exn)
    ]} *)

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

val peer : string Logs.Tag.def
