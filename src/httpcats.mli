(** HTTP client with Miou.

    A HTTP client using the Miou scheduler. It does a single HTTP request
    (though may follow redirects) to a remote uri. Both HTTP protocol 1.1 and
    2.0 are supported. Both http and https (via the pure implementation
    [ocaml-tls]) are supported. A connection is established via the
    happy-eyeballs algorithm.
*)

type error

val pp_error : error Fmt.t

module Version = Httpaf.Version
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

val request :
     ?config:[ `HTTP_1_1 of Httpaf.Config.t | `H2 of H2.Config.t ]
  -> ?tls_config:Tls.Config.client
  -> ?authenticator:X509.Authenticator.t
  -> ?meth:Httpaf.Method.t
  -> ?headers:(string * string) list
  -> ?body:string
  -> ?max_redirect:int
  -> ?follow_redirect:bool
  -> resolver:Happy.stack
  -> f:(response -> 'a -> string -> 'a)
  -> uri:string
  -> 'a
  -> (response * 'a, error) result

module Client = Http_miou_client
module Server = Http_miou_server

(**/**)

type uri =
  bool * string * (string * string option) option * string * int option * string

val decode_uri : string -> (uri, [> `Msg of string ]) result
