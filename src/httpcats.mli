(** HTTP client with Miou.

    [httpcats] is a HTTP client using the Miou scheduler. It does a single HTTP
    request (though may follow redirects) to a remote URI. Both HTTP protocol
    1.1 and 2.0 are supported. Both clear HTTP and HTTP with TLS (via the pure
    implementation [ocaml-tls]) are supported. A connection can be established
    via the happy-eyeballs algorithm if provided with a specific domain-name
    resolver (via [ocaml-dns]).

    The first entry point of [httpcats] is the {!val:request} function, which is
    used to make an HTTP request.

    An HTTP server is also available (see {!module:Server}) but offers an API
    for experienced users. We recommend using [Vif] if you would like to have an
    HTTP server. *)

(** {2:crypto [httpcats] and cryptography}

    When an HTTP request is made, it may require cryptographic calculations,
    which in turn require a random number generator. [httpcats] exclusively uses
    [mirage-crypto] (an OCaml implementation of cryptographic primitives, some
    of which have been proven via the [fiat-crypto] project) for all these
    cryptographic calculations.

    It is therefore necessary to initialise this random number generator
    according to the scheduler used (here Miou), and it is the sole
    responsibility of the end user to do so (furthermore, a library such as
    [httpcats] should not initialise global elements but leave control of these
    elements to the end user).

    In the case of Miou, [mirage-crypto] and [httpcats], you should initialise
    your application in this way:

    {[
      let () = Miou_unix.run @@ fun () ->
        let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
        let finally () = Mirage_crypto_rng_miou_unix.kill rng in
        Fun.protect ~finally @@ fun () ->
        Httpcats.request ...
    ]} *)

type error =
  [ `V1 of H1.Client_connection.error
  | `V2 of H2.Client_connection.error
  | `Protocol of string
  | `Msg of string
  | `Exn of exn ]
(** The type of errors. *)

val pp_error : error Fmt.t
(** Pretty-printer of {!error}s. *)

module Version = H1.Version
(** Protocol Version.

    Consists of [major.minor], in H2 this is [2.0]. *)

module Status = H2.Status
(** Response Status codes.

    A three-digit integer, the result of the request. *)

(** {2:headers Headers}

    The HTTP protocol requires that the request contain certain mandatory
    information in order to communicate correctly with a server. If this
    information is not present in the [~headers] passed to {!val:request},
    [httpcats] will add it. Here are the fields that [httpcats] can add:
    - [Host] or [:authority]
    - [User-Agent]
    - [Connection]
    - [Content-Length] or [Transfer-Encoding]

    The value of these fields depends on the arguments given to {!val:request}.

    [Host] or [:authority] allows you to specify which site you want to access.
    A server can offer several sites and requires you to specify which one you
    want to communicate with. [Host] is the required field for the [http/1.1]
    protocol and [:authority] is the required field for the [h2] protocol.

    [User-Agent] is an implementation identifier used to communicate with the
    server. It has the following format: [hurl/%%VERSION%%].

    The {!val:request} function of [httpcats] does {b not} handle request
    pooling. This means that {!val:request} only makes a single request. In this
    sense, [httpcats] always adds the [Connection] field to specify that the
    current connection with the server should be closed ([close]) as soon as the
    response has been transmitted.

    Finally, depending on the content that the user wants to send to the server,
    it is necessary to specify either the size ([Content-Length]) if the user is
    using {!val:string}, or specify that the content will be sent as a stream if
    the user is using {!val:stream}.

    The user can specify their own fields using the [~headers] option. This is a
    list containing the fields and their values. Case (lowercase or uppercase)
    and order do not matter, and duplicates are allowed (but, as mentioned
    above, [httpcats] does not generate duplicates). *)

module Headers = H2.Headers
(** Header fields.

    Case-insensitive key-value pairs. *)

module Method = H2.Method
(** Request methods. *)

type request = { meth: Method.t; target: string; headers: Headers.t }
(** A request consisting of a method (see {!module:Method}), a {i target} (the
    path requested by the client) and a headers. *)

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

type socket =
  [ `Tcp of Miou_unix.file_descr | `Tls of Tls_miou_unix.t ]
  * Ipaddr.t
  * int
  * Tls.Core.epoch_data option
(** A socket is a connection initiated with the requested HTTP server (from an
    URI). The socket is supplemented with information such as the TLS
    certificate used and the IP address (and the port) of the server to which we
    are connected. *)

(** {2:dns [httpcats] and domain name resolution}

    Establishing a connection to a server requires knowing its IP address. So,
    if you want to connect to [http://foo.bar], [httpcats] should resolve the
    domain name [foo.bar] in order to connect to the server. In this case, by
    default and to make [httpcats] easier to use, we use [gethostbyname(3P)] to
    resolve domain names.

    However, there are two problems with using this function:
    - you may be subject to censorship and unable to access certain sites that
      your Internet service provider's DNS resolver censors
    - the use of [gethostbyname(3P)] focuses primarily on IPv4 resolution,
      whereas [foo.bar] may also be available in IPv6.

    [httpcats] therefore also offers:
    + domain name resolution via our OCaml implementation: [ocaml-dns], where
      the user can specify the DNS resolver they wish to use
      ([uncensoreddns.org] is one solution)
    + an algorithm that prioritises connections via IPv6 and competes between
      different IPs to select the fastest: the {i happy-eyeballs} algorithm.

    Here is a complete example of how to use both {i happy-eyeballs} and
    [ocaml-dns] with the DNS resolver from [uncensoreddns.org]. Note that
    resolution is also encrypted using {i DNS-over-TLS}.

    {[
      let uncensoreddns_org =
        let ipaddr = Ipaddr.of_string_exn "89.233.43.71" in
        let authenticator = X509.Authenticator.of_string
          "key-fp:SHA256:INSZEZpDoWKiavosV2/xVT8O83vk/RRwS+LTiL+IpHs=" in
        let authenticator = Result.get_ok authenticator in
        let authenticator = authenticator time in
        let tls = Result.get_ok (Tls.Config.client ~authenticator ()) in
        `Tls (tls, ipaddr, 853)

      let getaddrinfo dns record domain_name =
        match record with
        | `A ->
          let r = Dns.Rr_map.A in
          let* (_ttl, set) = Dns_client_miou_unix.getaddrinfo dns r domain_name in
          let fn ipv4 = Ipaddr.Set.add (Ipaddr.V4 ipv4) in
          Ok (Ipaddr.V4.Set.fold fn set Ipaddr.Set.empty)
        | `AAAA ->
          let r = Dns.Rr_map.Aaaa in
          let* (_ttl, set) = Dns_client_miou_unix.getaddrinfo dns r domain_name in
          let fn ipv6 = Ipaddr.Set.add (Ipaddr.V6 ipv6) in
          Ok (Ipaddr.V6.Set.fold fn set Ipaddr.Set.empty)

      let () = Miou_unix.run @@ fun () ->
        let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
        let he = Happy_eyeballs_miou_unix.create () in
        let nameservers = [ uncensoreddns_org ] in
        let dns = Dns_client_miou_unix.create ~nameservers he in
        Happy_eyeballs_miou_unix.inject he (getaddrinfo dns);
        let finally () =
          Happy_eyeballs_miou_unix.kill he
          Mirage_crypto_rng_miou_unix.kill rng in
        Fun.protect ~finally @@ fun () ->
        Httpcats.request ~resolver:(`Happy_eyeballs he) ...
    ]}

    Firstly, it is entirely possible to use {i happy-eyeballs} without
    [ocaml-dns]. To do this, you do not need to {i inject} a new [getaddrinfo]
    into [he] (the {i happy-eyeballs} instance) and use it as is. In this case,
    {i happy-eyeballs} uses your system's DNS resolution, but you have the
    advantage of using an algorithm that not only prioritises IPv6 but also
    selects the fastest connection from all the IPs offered by your DNS
    resolver.

    [httpcats] also allows users to specify their own domain name resolution
    mechanism. To do this, the user must implement a function (see
    {!type:resolver}) which, depending on the port, whether or not TLS
    configuration is required (if we want to access http://foo.bar or
    https://foo.bar) and the host, must produce a {!type:socket} (associated
    with information about the protocols underlying HTTP). To do this, the user
    just needs to specify the [~resolver] argument on the {!val:request}
    function with [`User my_resolver].

    In this example, [uncensoreddns.org] is used as our DNS resolver. DNS
    queries to this resolver will be encrypted via TLS and will only accept a
    TLS certificate (specified by [uncensoreddns.org]
    {{:https://blog.uncensoreddns.org/dns-servers/}here}). *)

type resolver =
     ?port:int
  -> ?tls_config:Tls.Config.client
  -> string
  -> (socket, [ `Msg of string ]) result
(** A resolver is a function that, depending on the arguments (the port, whether
    we want to initiate a TLS connection, and the host), produces a
    {!type:socket} with information about the protocols underlying HTTP. If a
    TLS configuration is given, it means that [httpcats] expects a TLS
    connection with [ocaml-tls].

    {b NOTE}: The host given can be a domain name or an IP address in the form
    of a [string]. *)

(** {2:body The body of the request}

    [httpcats] allows you to transmit content via an HTTP request. This is
    particularly useful when transmitting a form using the POST method.
    [httpcats] expects two types of content:
    + a simple [string] (see {!val:string})
    + a {i stream} which is a [string Seq.t] (see {!val:stream})

    {3:stream The body as a stream.}

    If the user wishes to transfer a large amount of content, it is advisable to
    give [httpcats] a stream, i.e. a [string Seq.t], capable of producing parts
    of the content.

    {[
      let seq_of_filename filename =
        let ic = open_in_bin filename in
        let buf = Bytes.create 0x7ff in
        let rec go () =
          let len = input ic buf 0 (Bytes.length buf) in
          if len = 0 then
            let () = close_in ic in Seq.Nil
          else
            let str = Bytes.sub_string buf 0 len in
            Seq.Cons (str, go) in
        go

      let run () =
        let body = Httpcats.stream (seq_of_filename "form.txt") in
        Httpcats.request ~uri:"http://foo.bar" ~body ...
    ]}

    [httpcats] will add (if it does not already exist) information to the
    request stating that the content is being transferred in chunks (i.e.
    [httpcats] adds the header [Transfer-Encoding: Chunked]). For more
    information, please refer to the
    {{:https://en.wikipedia.org/wiki/Chunked_transfer_encoding}Wikipedia page}.

    The [Seq] module mentions two types of sequences:
    + {i persistent} sequences
    + {i ephemeral} sequences

    In the case of [httpcats] and possible redirects that may occur before
    reaching the final resource, there are cases where we would need to
    retransmit the content {b multiple times}. [htttpcats] therefore transforms
    all given streams into {i persistent} sequences using [Seq.memoize]. *)

(** A body, consisting to a basic string or a stream ([string Seq.t]). *)
type body = String of string | Stream of string Seq.t

val string : string -> body
(** [string str] is a {!type:body} from a string. *)

val stream : string Seq.t -> body
(** [stream seq] is a {!type:body} from a sequence of bytes. *)

(** {2:certificates [httpcats] and certificates}

    When communicating securely with a server, [ocaml-tls] attempts to validate
    the certificate presented by the server. The function used to validate the
    certificate is a value of type [X509.Authenticator.t].

    By default, [httpcats] will load your system's certificates using the
    [ca-certs] library and will attempt to find a {i chain of trust} between
    these certificates and the one announced by the server. This means that, by
    default, [httpcats] does not accept {i self-signed} certificates or
    certificates that are not linked to certificates available on your system.
    However, there are several ways to make [ocaml-tls] more permissive in
    certificate validation.

    {3 Accept anything and be unsecure}

    In a case where we want to iterate fairly quickly without considering
    TLS-related issues, we could accept all certificates without performing any
    validation. Here's how to do it:

    {[
      let run () =
        let authenticator ?ip:_ ~host:_ _ = Ok None in
        Httpcats.request ~authenticator ~uri:"https://foo.bar" ...
    ]}

    However, we {b do not recommend} using such an [X509.Authenticator.t] in
    production, as a certificate issued by a third party (such as an attacker)
    would also be accepted.

    {3 Self-signed certificat.}

    If you have a certificate, you can obtain its {i fingerprint} and generate
    an {i authenticator} from it:

    {[
      $ openssl x509 -noout -fingerprint -sha1 -inform pem -in cert.pem | \
        cut -d'=' -f2 | \
        tr -d ':'
      8C452106C58135CA638C1BF2AF019BB00A8A44B3
    ]}

    {[
      let run () =
        let authenticator = X509.Authenticator.cert_fingerprint
          ~time:(fun () -> Some (Ptime_clock.now ()))
          ~hash:`SHA1
          "8C452106C58135CA638C1BF2AF019BB00A8A44B3" in
        Httpcats.request ~authenticator ~uri:"https://foo.bar" ...
    ]}

    If your server uses the [cert.pem] certificate, [httpcats] and [ocaml-tls]
    will verify that this is indeed the certificate being advertised (and an
    attacker cannot corrupt the communication).

    {3 Generate and use in-the-fly certificate.}

    Since we are using [ocaml-tls], it is also possible to generate a
    certificate {i in the fly}. This means that you do not need to generate one
    with, for example, [openssl] and then manipulate it to obtain the
    information necessary for [httpcats] to communicate with your server. It is
    entirely possible to generate a certificate directly in OCaml, initiate a
    server using [httpcats] and this certificate, and obtain the
    {i authenticator} to communicate with this initiated server. Here is a
    complete example of how to do this:

    {[
      let _10s = Ptime.Span.of_int_s 10

      let ca_name =
        X509.Distinguished_name.
          [ Relative_distinguished_name.singleton (CN "httpcats") ]

      let certificate domain_name =
        let ( let* ) = Result.bind in
        let pk = Mirage_crypto_pk.Rsa.generate ~bits:2048 () in
        let pk = `RSA pk in
        let valid_from = Ptime.(sub_span (v (Ptime_clock.now_d_ps ())) _10s) in
        let valid_from = Option.get valid_from in
        let valid_until = Ptime.add_span valid_from (Ptime.Span.v (365, 0L)) in
        let valid_until = Option.get valid_until in
        let* ca_csr = X509.Signing_request.create ca_name pk in
        let extensions =
          let open X509.Extension in
          let pk = (X509.Signing_request.info ca_csr).public_key in
          let key_id = X509.Public_key.id pk in
          let domain_name = Domain_name.to_string domain_name in
          let alt_name = X509.General_name.(singleton DNS [ domain_name ]) in
          let usage =
            [ `Digital_signature; `Content_commitment; `Key_encipherment ]
          in
          empty
          |> add Subject_alt_name (true, alt_name)
          |> add Basic_constraints (true, (false, None))
          |> add Key_usage (true, usage)
          |> add Subject_key_id (false, key_id)
        in
        let* certificate =
          X509.Signing_request.sign ~valid_from ~valid_until ~extensions ca_csr
            pk ca_name
        in
        let fingerprint = X509.Certificate.fingerprint `SHA256 certificate in
        let time () = Some (Ptime_clock.now ()) in
        let authenticator =
          X509.Authenticator.cert_fingerprint ~time ~hash:`SHA256 ~fingerprint
        in
        Ok (certificate, pk, authenticator)

      let run domain_name =
        let ( let* ) = Result.bind in
        let* cert, pk, authenticator = certificate domain_name in
        let server = Miou.call @@ fun () -> Httpcats.Server.with_tls ... in
        Httpcats.request ~authenticator ...
    ]} *)

(** {2:alpn [httpcats] and ALPN negotiation}

    There are two protocols for obtaining resources via HTTP: the [http/1.1]
    protocol and the [h2] protocol. [httpcats] manages both protocols and can
    perform what is known as ALPN negotiation to choose one of these two
    protocols. This negotiation {b only} takes place via TLS. Some servers only
    implement one of the two protocols (often [http/1.1]). However, [httpcats]
    always prioritises the [h2] protocol by default.

    The user can also {i force} the use of one of the two protocols by
    specifying a configuration for the [http/1.1] protocol (using [H1.Config.t]
    and [`HTTP_1_1]) or a configuration for the [h2] protocol (using
    [H2.Config.t] and [`H2]).

    If TLS is not involved in the communication, the [http/1.1] protocol will
    always be chosen. *)

(** {2:handler Response handler}

    [httpcats] expects a function [fn] that is capable of handling {b multiple}
    responses, with {i chunks} corresponding to the body of these responses. In
    the simplest case, the user only has to handle a single response, which is
    given to the function [fn] and must {i consume} the content of the response.
    Here is a practical example of how to obtain the response and its content:

    {[
      let fn _meta _req _resp buf chunk =
        match chunk with
        | Some str ->
            Buffer.add_string buf str;
            buf
        | None -> buf
      in
      let buf = Buffer.create 0x100 in
      let uri = "http://foo.bar" in
      let result = Httpcats.request ~follow_redirect:false ~fn ~uri buf in
      match result with
      | Ok (response, buf) ->
          let contents = Buffer.contents buf in
          Ok (response, contents)
      | Error _ as err -> err
    ]}

    The [fn] function has several arguments, such as:
    - [meta], which corresponds to information related to the protocols
      underlying HTTP (TCP/IP and TLS), see {!type:meta}.
    - [request], which is the request sent by [httpcats] (possibly containing
      new information described {{!section:headers}here}), see {!type:request}.
    - [response], which is the response given by the HTTP server, see
      {!type:response}.
    - [buf] or [acc], which is the accumulator given by the user. It can be a
      value of any types (['a]).
    - [chunk], which is a part of the response content that the user should save
      or process. If the value is [None], it means that the {b current} response
      no longer has any content.

    However, redirects may occur. As already explained (see {!section:body}), we
    need to forward the same content to the redirect. In this case, [fn] will be
    executed several times for all responses received throughout the redirects.

    Here is an example that aggregates all responses and their content in the
    form of a list in the case of one or more redirects:

    {[
      let fn _meta _req resp state chunk =
        match (state, chunk) with
        | `Body (_, []) -> assert false
        | `Body (chunks, resp :: resps), None ->
            let contents = String.concat "" (List.rev chunks) in
            let resp = (resp, contents) in
            `Responses (resp :: resps)
        | `Body (chunks, resps), Some str -> `Body (str :: chunks, resps)
        | `Responses resps, Some str ->
            let resps = (resp, "") :: resps in
            `Body ([ str ], resps)
        | `Responses resps, None ->
            let resps = (resp, "") :: resps in
            `Response resps
      in
      let uri = "http://foo.bar" in
      let result = Httpcats.request ~fn ~uri (`Response []) in
      match result with
      | Ok (_, `Responses resps) -> Ok resps
      | Ok (_, `Body _) -> assert false
      | Error _ as err -> err
    ]}

    It is also possible to simply filter the responses and only process the
    final response.

    {[
      let fn _meta _req resp buf chunk =
        if Httpcats.Status.is_redirection resp.status = false
        then match chunk with
          | Some str -> Buffer.add_string buf str; buf
          | None -> buf
        else buf in
      let uri = "http://foo.bar" in
      let buf = Buffer.create 0x100 in
      Httpcats.request ~fn ~uri buf ...
    ]} *)

type meta = (Ipaddr.t * int) * Tls.Core.epoch_data option
(** It may be interesting to know where the response comes from (the server's IP
    address and the configuration chosen during the TLS handshake). In this
    sense, all this information is condensed into the {i meta} type and given to
    the {{!section:handler}response handler}. *)

type 'a handler = meta -> request -> response -> 'a -> string option -> 'a
(** Type of response handlers (see {{!section:handler} this section} for more
    details). *)

(** {2:redirections [httpcats] and redirections}

    [httpcats] can handle redirects and bring the user directly to the final
    response. We recommend that you learn how [httpcats]
    {{!section:body} handles} the content of your request during a redirect, as
    well as how to {{!section:handler} handle} multiple responses within your
    {i handler}.

    By default, [httpcats] attempts a maximum of 5 redirects. This parameter can
    be changed with the [~max_redirect] option. The user can also prevent
    [httpcats] from following redirects (default behaviour) by specifying
    [~follow_redirect:false]. *)

(** {2:cookies [httpcats] and cookies}

    There are redirection patterns where the server attempts to save a cookie
    and redirect the user to another resource. In this case, it is necessary for
    [httpcats] to keep the cookies from the first response in order to send them
    back via the next request to the proposed redirection.

    The user can {i filter} these cookies throughout the redirects and thus keep
    some and delete others. The [~filter] argument allows you to specify what
    you want to keep and what you want to reject between the cookies currently
    used by [httpcats] and those that the server wants to add.

    By default, [httpcats] keeps the latest version of all cookies given by the
    server (whether they have expired or not). *)

type filter =
  (string * string) list -> Cookie.cookie list -> (string * string) list
(** Type of functions to filter cookies. *)

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
  -> ?cookies:filter
  -> fn:'a handler
  -> uri:string
  -> 'a
  -> (response * 'a, [> error ]) result
(** [request] allows you to make an HTTP request and obtain the response.
    Several options are available, all of which are described above. Here is a
    summary of the options and the associated sections explaining their uses in
    detail:
    - [?config] is useful if you need to force a particular protocol with the
      server (see {!section:alpn}).
    - [?tls_config] allows you to specify a TLS configuration that takes
      precedence over anything [httpcats] can infer about this protocol
      (including which certificates we should accept or ALPN negotiation).
    - [?authenticator] allows you to specify how you would like to validate
      certificates during TLS communication (see {!section:certificates}).
    - [?meth] allows you to specify the HTTP method you would like to use (see
      {!module:Method}).
    - [?headers] allows you to specify the fields and their values that you want
      to send to the server (see {!section:headers}).
    - [?body] allows you to specify the content of your request (see
      {!section:body}).
    - [?max_redirect] & [?follow_redirect] specify how [httpcats] behaves with
      regard to redirection (see {!section:redirections}).
    - [?resolver] allows you to specify the domain name resolution mechanism
      (see {!section:dns})
    - [?cookies] allows the user to control which cookies must be kept during
      redirections (see {!section:cookies}).
    - [fn] & ['a] handles the responses received by the server (see
      {!section:handler}).
    - [uri] is the target of your request (for example, [https://foo.bar/]).

    It is {b mandatory} to initialise a random number generator (see
    {!section:crypto} before using [request] (which may involve cryptographic
    calculations). *)

module Client = Http_miou_client
module Server = Http_miou_server
module Cookie = Cookie

module Flow = Flow
(** [Flow] is the interface required by [httpcats] to implement the HTTP client
    and the HTTP server. This interface is really close to what [Unix] can
    provide. *)

module Miou_flow = Http_miou_unix
(** [Miou_flow] provides an implementation of {!Flow.S} from a TCP/IP connection
    and an implementation of {!Flow.S} from a TLS connection (with [ocaml-tls]).
    These are the implementations used by the HTTP client and HTTP server
    implementations. *)

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
