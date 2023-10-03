type error

val pp_error : error Fmt.t

module Version = Httpaf.Version
module Status = H2.Status
module Headers = H2.Headers

type response = {
  version : Version.t;
  status : Status.t;
  reason : string;
  headers : Headers.t;
}

val request :
  ?config:[ `HTTP_1_1 of Httpaf.Config.t | `H2 of H2.Config.t ] ->
  ?tls_config:Tls.Config.client ->
  ?authenticator:X509.Authenticator.t ->
  ?meth:Httpaf.Method.t ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?max_redirect:int ->
  ?follow_redirect:bool ->
  resolver:Happy.stack ->
  f:('a -> string -> 'a) ->
  uri:string ->
  'a ->
  (response * 'a, error) result
