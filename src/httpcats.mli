(** HTTP client with Miou.

    A HTTP client using the Miou scheduler. It does a single HTTP request
    (though may follow redirects) to a remote uri. Both HTTP protocol 1.1 and
    2.0 are supported. Both http and https (via the pure implementation
    [ocaml-tls]) are supported. A connection is established via the
    happy-eyeballs algorithm if provided. *)

module Flow = Flow
module Miou_flow = Http_miou_unix

type error =
  [ `V1 of H1.Client_connection.error
  | `V2 of H2.Client_connection.error
  | `Protocol of string
  | `Msg of string
  | `Exn of exn ]

val pp_error : error Fmt.t
(** Pretty-printer of {!error}s. *)

module Version = H1.Version
(** Protocol Version

    Consists of [major.minor], in H2 this is [2.0]. *)

module Status = H2.Status
(** Response Status codes

    A three-digit integer, the result of the request. *)

module Headers = H2.Headers
(** Header fields

    Case-insensitive key-value pairs. *)

type response = {
    version: Version.t
  ; status: Status.t
  ; reason: string
  ; headers: Headers.t
}
(** A response, consisting of version, status, reason (HTTP 1.1 only), and
    headers. *)

val pp_response : response Fmt.t
(** Pretty printer of a {!response}. *)

(** A body, consisting to a basic string or a stream ([string Seq.t]). The last
    implies a
    {{:https://en.wikipedia.org/wiki/Chunked_transfer_encoding}“Chunked”}
    transmission if not specified in the headers. *)
type body = String of string | Stream of string Seq.t

type meta = (Ipaddr.t * int) * Tls.Core.epoch_data option
(** It may be interesting to know where the response comes from (the server's IP
    address and the configuration chosen during the TLS handshake). In this
    sense, all this information is condensed into the meta type. *)

type 'a handler = meta -> response -> 'a -> string option -> 'a
(** The handler is a function that is called each time a new part of the
    response body is retrieved. The end of the response content is notified by
    [None]. The user can then evolve a ['a] value between each call to the
    [handler] (like a buffer that gradually recovers the content of the
    response).

    {[
      let fn _meta _response buf = function
        | Some str -> Buffer.add_string buf str
        | None -> buf
    ]} *)

type socket =
  [ `Tcp of Miou_unix.file_descr | `Tls of Tls_miou_unix.t ]
  * Ipaddr.t
  * int
  * Tls.Core.epoch_data option

type resolver =
     ?port:int
  -> ?tls_config:Tls.Config.client
  -> string
  -> (socket, [ `Msg of string ]) result

val string : string -> body
val stream : string Seq.t -> body

val request :
     ?config:[ `HTTP_1_1 of H1.Config.t | `H2 of H2.Config.t ]
  -> ?tls_config:Tls.Config.client
  -> ?authenticator:X509.Authenticator.t
  -> ?meth:H1.Method.t
  -> ?headers:(string * string) list
  -> ?body:body
  -> ?max_redirect:int
  -> ?follow_redirect:bool
  -> ?resolver:
       [ `Happy of Happy_eyeballs_miou_unix.t | `User of resolver | `System ]
  -> f:'a handler
  -> uri:string
  -> 'a
  -> (response * 'a, [> error ]) result
(** [request] attempts to send a request to [uri]. Several arguments can be
    defined by the user or left as default. Mandatory arguments are:
    - the [uri] target of the request
    - the function [fn] handling the content of the response
    - the value [acc] to be passed to the function [fn] handling the response
      content.

    You can specify the query method via the [meth] argument. By default, the
    [GET] method is chosen (see {!module:H2.Method.t} for more details).

    The user can specify fields (via [headers]) in addition to those added by
    [httpcats] - [httpcats] will never replace your defined fields. By default,
    [httpcats] attempts to set [User-Agent], [Host] (according to the given
    [uri]), [Content-Length] & [Connection] or [Transfer-Encoding] according to
    the [body] argument. [:authority] is also added if you wish to use version 2
    of the HTTP protocol.

    The user may want to send content (using [POST]). The user can specify a
    [body] value, which can be a simple [string] (see {!val:string}) or a stream
    of bytes (see {!val:stream}). In the latter case, a
    {i chunked transfer encoding} is preferred by [httpcats].

    It may happen that the resource is not directly accessible and that the
    server responds with a redirect. By default, [httpcats] follows redirects.
    It should be noted that, even if there is a redirect, the [body] will be
    sent. The user can:
    - not follow redirects, in which case [follow_redirect:false] must be
      specified
    - define an arbitrary number of redirects to follow (by default, [5]) using
      [max_redirect].

    Users can specify the protocol (via [config]) they want to use with the
    server. By default, [httpcats] handles [http/1.1] and [h2], but the user can
    force either protocol. At the same time, the user can also configure the
    size of the internal buffers used by the protocol implementation. [httpcats]
    will always use [http/1.1] if TLS is not used. [h2] is only available
    through TLS.

    Users can also configure TLS (version, ciphers used, etc.) via the
    [tls_config] option. The user can also specify the certificate acceptance
    policy via the [authenticator] argument to accept, for example, self-signed
    certificates.

    Finally, users can choose to use a pure OCaml DNS implementation to resolve
    domain names via a {i happy-eyeballs} resolver. *)

module Client = Http_miou_client
module Server = Http_miou_server

(**/**)

type uri =
  bool * string * (string * string option) option * string * int option * string

val decode_uri : string -> (uri, [> `Msg of string ]) result

val resolve_location :
  uri:string -> location:string -> (string, [> `Msg of string ]) result

val prepare_headers :
     ?config:[ `HTTP_1_1 of H1.Config.t | `H2 of H2.Config.t ]
  -> meth:H1.Method.t
  -> uri:string
  -> ?body:body
  -> (string * string) list
  -> ((string * string) list, [> `Msg of string ]) result
