let src = Logs.Src.create "httpcats"

module Log = (val Logs.src_log src : Logs.LOG)
module Flow = Flow
module Runtime = Runtime
module Client = Http_miou_client
module Server = Http_miou_server
module Version = H1.Version
module Status = H2.Status
module Headers = H2.Headers

let ( % ) f g x = f (g x)
let open_error = function Ok _ as v -> v | Error #Client.error as v -> v
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let open_error_msg = function Ok _ as v -> v | Error (`Msg _) as v -> v

let decode_host_port str =
  match String.split_on_char ':' str with
  | [] -> Error (`Msg "Empty host part")
  | [ host ] -> Ok (host, None)
  | [ host; "" ] -> Ok (host, None)
  | hd :: tl -> (
      let port, host =
        match List.rev (hd :: tl) with
        | hd :: tl -> (hd, String.concat ":" (List.rev tl))
        | _ -> assert false
      in
      try Ok (host, Some (int_of_string port))
      with _ -> Error (`Msg "Couln't decode port"))

let decode_user_pass up =
  match String.split_on_char ':' up with
  | [ user; pass ] -> Ok (user, Some pass)
  | [ user ] -> Ok (user, None)
  | _ -> assert false

type uri =
  bool * string * (string * string option) option * string * int option * string

let decode_uri uri =
  (* proto :// user : pass @ host : port / path *)
  let ( >>= ) = Result.bind in
  match String.split_on_char '/' uri with
  | proto :: "" :: user_pass_host_port :: path ->
      (if String.equal proto "http:" then Ok ("http", false)
       else if String.equal proto "https:" then Ok ("https", true)
       else Error (`Msg "Unknown protocol"))
      >>= fun (scheme, is_tls) ->
      (match String.split_on_char '@' user_pass_host_port with
      | [ host_port ] -> Ok (None, host_port)
      | [ user_pass; host_port ] ->
          decode_user_pass user_pass >>= fun up -> Ok (Some up, host_port)
      | _ -> Error (`Msg "Couldn't decode URI"))
      >>= fun (user_pass, host_port) ->
      decode_host_port host_port >>= fun (host, port) ->
      Ok (is_tls, scheme, user_pass, host, port, "/" ^ String.concat "/" path)
  | [ user_pass_host_port ] ->
      (match String.split_on_char '@' user_pass_host_port with
      | [ host_port ] -> Ok (None, host_port)
      | [ user_pass; host_port ] ->
          decode_user_pass user_pass >>= fun up -> Ok (Some up, host_port)
      | _ -> Error (`Msg "Couldn't decode URI"))
      >>= fun (user_pass, host_port) ->
      decode_host_port host_port >>= fun (host, port) ->
      Ok (false, "", user_pass, host, port, "/")
  | user_pass_host_port :: path ->
      (match String.split_on_char '@' user_pass_host_port with
      | [ host_port ] -> Ok (None, host_port)
      | [ user_pass; host_port ] ->
          decode_user_pass user_pass >>= fun up -> Ok (Some up, host_port)
      | _ -> Error (`Msg "Couldn't decode URI"))
      >>= fun (user_pass, host_port) ->
      decode_host_port host_port >>= fun (host, port) ->
      Ok (false, "", user_pass, host, port, "/" ^ String.concat "/" path)
  | _ -> Error (`Msg "Could't decode URI on top")

let resolve_location ~uri ~location =
  match String.split_on_char '/' location with
  | "http:" :: "" :: _ -> Ok location
  | "https:" :: "" :: _ -> Ok location
  | "" :: "" :: _ ->
      let schema = String.sub uri 0 (String.index uri '/') in
      Ok (schema ^ location)
  | "" :: _ -> begin
      match String.split_on_char '/' uri with
      | schema :: "" :: user_pass_host_port :: _ ->
          Ok (String.concat "/" [ schema; ""; user_pass_host_port ^ location ])
      | _ -> error_msgf "Expected an absolute uri, got: %S" uri
    end
  | _ -> error_msgf "Unknown location (relative path): %S" location

let add_authentication ?(meth = `Basic) ~add headers user_pass =
  match (user_pass, meth) with
  | None, _ -> headers
  | Some (user, Some pass), `Basic ->
      let data = Base64.encode_string (user ^ ":" ^ pass) in
      let str = "Basic " ^ data in
      add headers "authorization" str
  | Some (user, None), `Basic ->
      let data = Base64.encode_string user in
      let str = "Basic " ^ data in
      add headers "authorization" str

let user_agent = "hurl/%%VERSION_NUM%%"

type response = {
    version: Version.t
  ; status: Status.t
  ; reason: string
  ; headers: Headers.t
}

let pp_response ppf resp =
  Fmt.pf ppf
    "@[<hov>{ version=@ %a;@ status=@ %s;@ reason=@ %S;@ headers=@ @[<hov>%a@] \
     }@]"
    Version.pp_hum resp.version
    (Status.to_string resp.status)
    resp.reason Headers.pp_hum resp.headers

type error =
  [ `V1 of H1.Client_connection.error
  | `V2 of H2.Client_connection.error
  | `Protocol of string
  | `Msg of string
  | `Exn of exn ]

let pp_error ppf = function
  | `Msg msg -> Fmt.string ppf msg
  | #Client.error as err -> Client.pp_error ppf err

type body = String of string | Stream of string Seq.t
type meta = (Ipaddr.t * int) * Tls.Core.epoch_data option
type 'a handler = meta -> response -> 'a -> string option -> 'a

let prep_http_1_1_headers hdr host user_pass body =
  let hdr = H1.Headers.of_list hdr in
  let add = H1.Headers.add_unless_exists in
  let hdr = add hdr "user-agent" user_agent in
  let hdr = add hdr "host" host in
  let hdr =
    match body with
    | Some (Some len) ->
        let hdr = add hdr "connection" "close" in
        add hdr "content-length" (string_of_int len)
    | Some None -> add hdr "transfer-encoding" "chunked"
    | None ->
        let hdr = add hdr "connection" "close" in
        add hdr "content-length" "0"
  in
  add_authentication ~add hdr user_pass

let prep_h2_headers hdr (host : string) user_pass =
  (* please note, that h2 (at least in version 0.10.0) encodes the headers
     in reverse order ; and for http/2 compatibility we need to retain the
     :authority pseudo-header first (after method/scheme/... that are encoded
     specially *)
  (* also note that "host" is no longer a thing, but :authority is -- so if
     we find a host header, we'll rephrase that as authority. *)
  let hdr = List.rev_map (fun (k, v) -> (String.lowercase_ascii k, v)) hdr in
  let hdr = H2.Headers.of_rev_list hdr in
  let hdr, authority =
    match (H2.Headers.get hdr "host", H2.Headers.get hdr ":authority") with
    | None, None -> (hdr, host)
    | Some h, None -> (H2.Headers.remove hdr "host", h)
    | None, Some a -> (H2.Headers.remove hdr ":authority", a)
    | Some h, Some a ->
        if String.equal h a then
          let hdr = H2.Headers.remove hdr ":authority" in
          let hdr = H2.Headers.remove hdr "host" in
          (hdr, h)
        else (H2.Headers.remove hdr ":authority", a)
  in
  let add hdr = H2.Headers.add_unless_exists hdr ?sensitive:None in
  let hdr = H2.Headers.add_list H2.Headers.empty (H2.Headers.to_rev_list hdr) in
  let hdr = add hdr ":authority" authority in
  let hdr = add hdr "user-agent" user_agent in
  add_authentication ~add hdr user_pass

let from_h1 response =
  {
    version= response.H1.Response.version
  ; status= (response.H1.Response.status :> H2.Status.t)
  ; reason= response.H1.Response.reason
  ; headers=
      H2.Headers.of_list (H1.Headers.to_list response.H1.Response.headers)
  }

let from_h2 response =
  {
    version= { major= 2; minor= 0 }
  ; status= response.H2.Response.status
  ; reason= H2.Status.to_string response.H2.Response.status
  ; headers= response.H2.Response.headers
  }

let _is_a_valid_redirection resp ~uri =
  if Status.is_redirection resp.status then
    match Headers.get resp.headers "location" with
    | Some location ->
        let uri = resolve_location ~uri ~location in
        Result.is_ok uri
    | None -> false
  else false

type config = {
    meth: H2.Method.t
  ; headers: (string * string) list
  ; body: body option
  ; scheme: string
  ; user_pass: (string * string option) option
  ; host: string
  ; path: string
  ; ipaddr: Ipaddr.t
  ; port: int
  ; epoch: Tls.Core.epoch_data option
}

let[@warning "-8"] single_http_1_1_request ?(config = H1.Config.default) flow
    cfg ~f:fn acc =
  let contents_length =
    match cfg.body with
    | Some (String str) -> Some (Some (String.length str))
    | Some (Stream _) -> Some None
    | None -> None
  in
  let hdrs =
    prep_http_1_1_headers cfg.headers cfg.host cfg.user_pass contents_length
  in
  let request = H1.Request.create ~headers:hdrs cfg.meth cfg.path in
  let f (`V1 response : Client.response) acc str =
    let resp = from_h1 response in
    fn ((cfg.ipaddr, cfg.port), cfg.epoch) resp acc (Some str)
  in
  let finally () =
    match flow with
    | `Tls flow -> Tls_miou_unix.close flow
    | `Tcp flow -> Http_miou_unix.TCP.close flow
  in
  Fun.protect ~finally @@ fun () ->
  let (Client.Process { version= V1; acc; response; body; process }) =
    Client.run ~f acc (`V1 config) flow (`V1 request)
  in
  let go orphans =
    Log.debug (fun m -> m "Start to send something to the server.");
    let seq, to_close =
      match cfg.body with
      | Some (String str) -> (Seq.return str, true)
      | Some (Stream seq) -> (seq, true)
      | None -> (Seq.empty, false)
    in
    let send str =
      H1.Body.Writer.write_string body str;
      H1.Body.Writer.flush body Miou.yield;
      Miou.yield ()
    in
    Seq.iter send seq;
    Log.debug (fun m -> m "body sent (through http/1.1), close it (%b)" to_close);
    if to_close then H1.Body.Writer.close body;
    Runtime.terminate orphans
  in
  let sender = Miou.async @@ fun () -> Runtime.flat_tasks go in
  let on_error = function Client.Error err -> err | exn -> `Exn exn in
  let ( let* ) = Result.bind in
  let* () = Result.map_error on_error (Miou.await sender) in
  let* () = Result.map_error on_error (Miou.await process) in
  let* resp =
    Result.map_error (on_error % fst) (Miou.Computation.await response)
  in
  let resp = from_h1 resp in
  Ok (resp, fn ((cfg.ipaddr, cfg.port), cfg.epoch) resp !acc None)

let[@warning "-8"] single_h2_request ?(config = H2.Config.default) flow cfg
    ~f:fn acc =
  let hdrs = prep_h2_headers cfg.headers cfg.host cfg.user_pass in
  Log.debug (fun m -> m "h2 headers: @[<hov>%a@]" H2.Headers.pp_hum hdrs);
  let request =
    H2.Request.create ~scheme:cfg.scheme ~headers:hdrs cfg.meth cfg.path
  in
  let f (`V2 response : Client.response) acc str =
    fn ((cfg.ipaddr, cfg.port), cfg.epoch) (from_h2 response) acc (Some str)
  in
  let (Client.Process { version= V2; acc; response; body; process }) =
    Client.run ~f acc (`V2 config) flow (`V2 request)
  in
  let go orphans =
    let seq =
      match cfg.body with
      | Some (String str) -> Seq.return str
      | Some (Stream seq) -> seq
      | None -> Seq.empty
    in
    let send str =
      H2.Body.Writer.write_string body str;
      H2.Body.Writer.flush body (fun _reason -> Miou.yield ());
      Miou.yield ()
    in
    Seq.iter send seq;
    Log.debug (fun m -> m "body sent (through h2), close it");
    H2.Body.Writer.close body; Runtime.terminate orphans
  in
  let sender = Miou.async @@ fun () -> Runtime.flat_tasks go in
  let on_error = function Client.Error err -> err | exn -> `Exn exn in
  let ( let* ) = Result.bind in
  let* () = Result.map_error on_error (Miou.await sender) in
  let* () = Result.map_error on_error (Miou.await process) in
  let* resp =
    Result.map_error (on_error % fst) (Miou.Computation.await response)
  in
  let resp = from_h2 resp in
  Ok (resp, fn ((cfg.ipaddr, cfg.port), cfg.epoch) resp !acc None)

let alpn_protocol = function
  | `Tcp _ -> None
  | `Tls tls -> (
      match Tls_miou_unix.epoch tls with
      | Some { Tls.Core.alpn_protocol= Some "h2"; _ } -> Some `H2
      | Some { Tls.Core.alpn_protocol= Some "http/1.1"; _ } -> Some `HTTP_1_1
      | Some { Tls.Core.alpn_protocol= None; _ } -> None
      | Some { Tls.Core.alpn_protocol= Some _; _ } -> None
      | None -> None)

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

let connect_system ?port ?tls_config host =
  let port =
    match (port, tls_config) with
    | None, None -> 80
    | None, Some _ -> 443
    | Some port, _ -> port
  in
  Log.debug (fun m -> m "try to connect to %s (with system)" host);
  match Unix.gethostbyname host with
  | exception Unix.Unix_error (err, f, v) ->
      error_msgf "%s(%s): %s" f v (Unix.error_message err)
  | { Unix.h_addr_list= [||]; _ } -> error_msgf "Impossible to resolve %s" host
  | { Unix.h_addr_list; _ } -> (
      let addr = Unix.ADDR_INET (h_addr_list.(0), port) in
      let socket =
        if Unix.is_inet6_addr h_addr_list.(0) then Miou_unix.tcpv6 ()
        else Miou_unix.tcpv4 ()
      in
      let timeout = Miou.async @@ fun () -> Miou_unix.sleep 5.0; `Timeout in
      let connect =
        Miou.async @@ fun () ->
        Miou_unix.connect socket addr;
        `Connected
      in
      match Miou.await_first [ timeout; connect ] with
      | Error exn ->
          Miou_unix.close socket;
          error_msgf "Got an unexpected exception while connecting: %s"
            (Printexc.to_string exn)
      | Ok `Timeout ->
          Miou_unix.close socket;
          error_msgf "Connection to %s (via %s:%d) timeout" host
            (Unix.string_of_inet_addr h_addr_list.(0))
            port
      | Ok `Connected -> begin
          let ipaddr = Ipaddr_unix.of_inet_addr h_addr_list.(0) in
          match tls_config with
          | Some tls_config ->
              let tls = Tls_miou_unix.client_of_fd tls_config socket in
              let epoch = Tls_miou_unix.epoch tls in
              Ok (`Tls tls, ipaddr, port, epoch)
          | None -> Ok (`Tcp socket, ipaddr, port, None)
        end)

let connect_happy_eyeballs ?port ?tls_config ~happy_eyeballs host =
  let port =
    match (port, tls_config) with
    | None, None -> 80
    | None, Some _ -> 443
    | Some port, _ -> port
  in
  Log.debug (fun m -> m "try to connect to %s (with happy-eyeballs)" host);
  match
    (Happy_eyeballs_miou_unix.connect happy_eyeballs host [ port ], tls_config)
  with
  | Ok ((ipaddr, port), file_descr), None ->
      Ok (`Tcp file_descr, ipaddr, port, None)
  | Ok ((ipaddr, port), file_descr), Some tls_config ->
      let tls = Tls_miou_unix.client_of_fd tls_config file_descr in
      let epoch = Tls_miou_unix.epoch tls in
      Ok (`Tls tls, ipaddr, port, epoch)
  | (Error _ as err), _ -> err

type config_for_a_request = {
    resolver:
      [ `Happy of Happy_eyeballs_miou_unix.t | `User of resolver | `System ]
  ; http_config: [ `HTTP_1_1 of H1.Config.t | `H2 of H2.Config.t ] option
  ; tls_config: (tls_config, error) result
  ; meth: H2.Method.t
  ; headers: (string * string) list
  ; body: body option
  ; uri: string
}

and tls_config = [ `Custom of Tls.Config.client | `Default of Tls.Config.client ]

let single_request cfg ~f acc =
  let ( let* ) = Result.bind in
  let ( let+ ) x f = Result.map f x in
  let* tls, scheme, user_pass, host, port, path = decode_uri cfg.uri in
  let* tls_config =
    if tls then
      let+ tls_config = cfg.tls_config in
      let host =
        let* domain_name = Domain_name.of_string host in
        Domain_name.host domain_name
      in
      match (tls_config, host) with
      | `Custom cfg, _ -> Some cfg
      | `Default cfg, Ok host -> Some (Tls.Config.peer cfg host)
      | `Default cfg, _ -> Some cfg
    else Ok None
  in
  Log.debug (fun m -> m "connect to %s (connected)" cfg.uri);
  let* flow, ipaddr, port, epoch =
    match cfg.resolver with
    | `Happy happy_eyeballs ->
        connect_happy_eyeballs ?port ?tls_config ~happy_eyeballs host
        |> open_error_msg
    | `User connect -> connect ?port ?tls_config host |> open_error_msg
    | `System -> connect_system ?port ?tls_config host |> open_error_msg
  in
  Log.debug (fun m -> m "single request to %s (connected)" cfg.uri);
  let cfg' =
    {
      meth= cfg.meth
    ; headers= cfg.headers
    ; body= cfg.body
    ; scheme
    ; user_pass
    ; host
    ; path
    ; ipaddr
    ; port
    ; epoch
    }
  in
  let result =
    match (alpn_protocol flow, cfg.http_config) with
    | (Some `HTTP_1_1 | None), Some (`HTTP_1_1 config) ->
        single_http_1_1_request ~config flow cfg' ~f acc
    | (Some `HTTP_1_1 | None), None -> single_http_1_1_request flow cfg' ~f acc
    | Some `H2, Some (`H2 config) -> single_h2_request ~config flow cfg' ~f acc
    | None, Some (`H2 _) ->
        Log.warn (fun m ->
            m "no ALPN protocol (choose http/1.1) where user forces h2");
        single_http_1_1_request flow cfg' ~f acc
    | Some `H2, None -> single_h2_request flow cfg' ~f acc
    | Some `H2, Some (`HTTP_1_1 _) ->
        Log.warn (fun m -> m "ALPN protocol is h2 where user forces http/1.1");
        single_h2_request flow cfg' ~f acc
    | Some `HTTP_1_1, Some (`H2 _) ->
        Log.warn (fun m -> m "ALPN protocol is http/1.1 where user forces h2");
        single_http_1_1_request flow cfg' ~f acc
  in
  open_error result

let string str = String str
let stream seq = Stream seq

(* NOTE(dinosaure): we must [memoize] (and ensure that the given [seq] is used
   only one time) the body if we follow redirections where we will try, for each
   redirections, to send the body. *)
let memoize = function
  | String _ as body -> body
  | Stream seq -> Stream (Seq.memoize seq)

let request ?config:http_config ?tls_config ?authenticator ?(meth = `GET)
    ?(headers = []) ?body ?(max_redirect = 5) ?(follow_redirect = true)
    ?(resolver = `System) ~f ~uri acc =
  let tls_config =
    match tls_config with
    | Some cfg -> Ok (`Custom cfg)
    | None ->
        let alpn_protocols =
          match http_config with
          | None -> [ "h2"; "http/1.1" ]
          | Some (`H2 _) -> [ "h2" ]
          | Some (`HTTP_1_1 _) -> [ "http/1.1" ]
        and authenticator =
          match authenticator with
          | None -> Ca_certs.authenticator ()
          | Some authenticator -> Ok authenticator
        in
        Result.map
          begin
            fun authenticator ->
              Tls.Config.client ~alpn_protocols ~authenticator ()
              |> Result.get_ok
              |> fun default -> `Default default
          end
          authenticator
  in
  let cfg =
    {
      resolver
    ; http_config
    ; tls_config
    ; meth
    ; headers
    ; body= Option.map memoize body
    ; uri
    }
  in
  if not follow_redirect then single_request cfg ~f acc
  else
    let ( let* ) = Result.bind in
    let rec go count uri =
      if count = 0 then Error (`Msg "Redirect limit exceeded")
      else
        let cfg = { cfg with uri; body= Option.map memoize cfg.body } in
        match single_request cfg ~f acc with
        | Error _ as err -> err
        | Ok (resp, result) ->
            if Status.is_redirection resp.status then
              match Headers.get resp.headers "location" with
              | Some location ->
                  let* uri = resolve_location ~uri ~location in
                  go (pred count) uri
              | None -> Ok (resp, result)
            else Ok (resp, result)
    in
    go max_redirect uri

let prepare_headers ?config:(version = `HTTP_1_1 H1.Config.default) ~uri ?body
    headers =
  let ( let+ ) x f = Result.map f x in
  let+ _tls, _scheme, user_pass, host, _port, _path = decode_uri uri in
  match version with
  | `HTTP_1_1 _ ->
      let blen =
        match body with
        | Some (String str) -> Some (Some (String.length str))
        | Some (Stream _) -> Some None
        | None -> None
      in
      H1.Headers.to_list (prep_http_1_1_headers headers host user_pass blen)
  | `H2 _ -> H2.Headers.to_list (prep_h2_headers headers host user_pass)
