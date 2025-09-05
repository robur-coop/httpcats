module Version = H1.Version
module Status = H2.Status
module Headers = H2.Headers
module Method = H2.Method
module Cookie = Cookie

type request = { meth: Method.t; target: string; headers: Headers.t }

type response = {
    version: Version.t
  ; status: Status.t
  ; reason: string
  ; headers: Headers.t
}

type error =
  [ `V1 of H1.Client_connection.error
  | `V2 of H2.Client_connection.error
  | `Protocol of string
  | `Msg of string
  | `Exn of exn ]

type body = String of string | Stream of string Seq.t
type meta = (Ipaddr.t * int) * Tls.Core.epoch_data option
type 'a handler = meta -> request -> response -> 'a -> string option -> 'a

module Server = struct
  type error =
    [ `V1 of H1.Server_connection.error
    | `V2 of H2.Server_connection.error
    | `Protocol of string ]

  type request = {
      meth: Method.t
    ; target: string
    ; scheme: string
    ; headers: Headers.t
  }

  type response = { status: Status.t; headers: Headers.t }
  type body = [ `V1 of H1.Body.Writer.t | `V2 of H2.Body.Writer.t ]
  type reqd = [ `V1 of H1.Reqd.t | `V2 of H2.Reqd.t ]

  type error_handler =
    [ `V1 | `V2 ] -> ?request:request -> error -> (Headers.t -> body) -> unit

  type 'a handler = 'a -> reqd -> unit
end
