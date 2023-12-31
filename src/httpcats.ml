let src = Logs.Src.create "http-client"

module Log = (val Logs.src_log src : Logs.LOG)

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

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

let user_agent = "http-client/%%VERSION_NUM%%"

let prep_http_1_1_headers headers host user_pass blen =
  let headers = Httpaf.Headers.of_list headers in
  let add = Httpaf.Headers.add_unless_exists in
  let headers = add headers "user-agent" user_agent in
  let headers = add headers "host" host in
  let headers = add headers "connection" "close" in
  let headers =
    add headers "content-length" (string_of_int (Option.value ~default:0 blen))
  in
  add_authentication ~add headers user_pass

let prep_h2_headers headers (host : string) user_pass blen =
  (* please note, that h2 (at least in version 0.10.0) encodes the headers
     in reverse order ; and for http/2 compatibility we need to retain the
     :authority pseudo-header first (after method/scheme/... that are encoded
     specially *)
  (* also note that "host" is no longer a thing, but :authority is -- so if
     we find a host header, we'll rephrase that as authority. *)
  let headers =
    List.rev_map (fun (k, v) -> (String.lowercase_ascii k, v)) headers
  in
  let headers = H2.Headers.of_rev_list headers in
  let headers, authority =
    match
      (H2.Headers.get headers "host", H2.Headers.get headers ":authority")
    with
    | None, None -> (headers, host)
    | Some h, None ->
        Log.debug (fun m ->
            m "removing host header (inserting authority instead)");
        (H2.Headers.remove headers "host", h)
    | None, Some a -> (H2.Headers.remove headers ":authority", a)
    | Some h, Some a ->
        if String.equal h a then
          (H2.Headers.remove (H2.Headers.remove headers ":authority") "host", h)
        else begin
          Log.warn (fun m ->
              m "authority header %s mismatches host %s (keeping both)" a h);
          (H2.Headers.remove headers ":authority", a)
        end
  in
  let add hdr = H2.Headers.add_unless_exists hdr ?sensitive:None in
  let hdr = H2.Headers.empty in
  let hdr = add hdr ":authority" authority in
  let hdr = H2.Headers.add_list hdr (H2.Headers.to_rev_list headers) in
  let hdr = add hdr "user-agent" user_agent in
  let hdr =
    add hdr "content-length" (string_of_int (Option.value ~default:0 blen))
  in
  add_authentication ~add hdr user_pass

module Version = Httpaf.Version
module Status = H2.Status
module Headers = H2.Headers

type response =
  { version : Version.t
  ; status : Status.t
  ; reason : string
  ; headers : Headers.t
  }

type error =
  [ `V1 of Httpaf.Client_connection.error
  | `V2 of H2.Client_connection.error
  | `Protocol of string
  | `Msg of string
  | `Tls of Http_miou_unix.TLS.error ]

let pp_error ppf = function
  | `Protocol msg -> Fmt.string ppf msg
  | `Msg msg -> Fmt.string ppf msg
  | `Tls err -> Http_miou_unix.TLS.pp_error ppf err
  | `V1 (`Malformed_response msg) ->
      Fmt.pf ppf "Malformed HTTP/1.1 response: %s" msg
  | `V1 (`Invalid_response_body_length _resp) ->
      Fmt.pf ppf "Invalid response body length"
  | `V1 (`Exn exn) | `V2 (`Exn exn) ->
      Fmt.pf ppf "Got an unexpected exception: %S" (Printexc.to_string exn)
  | `V2 (`Malformed_response msg) -> Fmt.pf ppf "Malformed H2 response: %s" msg
  | `V2 (`Invalid_response_body_length _resp) ->
      Fmt.pf ppf "Invalid response body length"
  | `V2 (`Protocol_error (err, msg)) ->
      Fmt.pf ppf "Protocol error %a: %s" H2.Error_code.pp_hum err msg

let from_httpaf response =
  { version = response.Httpaf.Response.version
  ; status = (response.Httpaf.Response.status :> H2.Status.t)
  ; reason = response.Httpaf.Response.reason
  ; headers =
      H2.Headers.of_list
        (Httpaf.Headers.to_list response.Httpaf.Response.headers)
  }

let single_http_1_1_request ?(config = Httpaf.Config.default) flow user_pass
    host meth path headers body f acc =
  let body_length = Option.map String.length body in
  let headers = prep_http_1_1_headers headers host user_pass body_length in
  let request = Httpaf.Request.create ~headers meth path in
  let f response acc str =
    let[@warning "-8"] (`V1 response : Http_miou_client.response) = response in
    f (from_httpaf response) acc str
  in
  Log.debug (fun m -> m "start to send the http/1.1 request");
  match Http_miou_client.run ~f acc (`V1 config) flow (`V1 request) with
  | Process (V2, _, _) -> assert false
  | Process (V1, await, { body = stream; write_string; close; release }) -> (
      Option.iter (write_string stream) body;
      close stream;
      let result = await () in
      release ();
      match result with
      | Ok (response, acc) -> Ok (from_httpaf response, acc)
      | Error (#Http_miou_client.error as err) -> Error (err :> error))

let from_h2 response =
  { version = { major = 2; minor = 0 }
  ; status = response.H2.Response.status
  ; reason = ""
  ; headers = response.H2.Response.headers
  }

let single_h2_request ?(config = H2.Config.default) flow scheme user_pass host
    meth path headers body f acc =
  let body_length = Option.map String.length body in
  let headers = prep_h2_headers headers host user_pass body_length in
  let request = H2.Request.create ~scheme ~headers meth path in
  let f response acc str =
    let[@warning "-8"] (`V2 response : Http_miou_client.response) = response in
    f (from_h2 response) acc str
  in
  match Http_miou_client.run ~f acc (`V2 config) flow (`V2 request) with
  | Process (V1, _, _) -> assert false
  | Process (V2, await, { body = stream; write_string; close; release }) -> (
      Option.iter (write_string stream) body;
      close stream;
      let result = await () in
      release ();
      match result with
      | Ok (response, acc) -> Ok (from_h2 response, acc)
      | Error (#Http_miou_client.error as err) -> Error (err :> error))

let alpn_protocol = function
  | `Tcp _ -> None
  | `Tls tls -> (
      match Http_miou_unix.epoch tls with
      | Some { Tls.Core.alpn_protocol = Some "h2"; _ } -> Some `H2
      | Some { Tls.Core.alpn_protocol = Some "http/1.1"; _ } -> Some `HTTP_1_1
      | Some { Tls.Core.alpn_protocol = None; _ } -> None
      | Some { Tls.Core.alpn_protocol = Some protocol; _ } ->
          Log.warn (fun m ->
              m "The ALPN negotiation unexpectedly resulted in %S." protocol);
          None
      | None -> None)

let connect ?port ?tls_config ~happy_eyeballs host =
  let port =
    match (port, tls_config) with
    | None, None -> 80
    | None, Some _ -> 443
    | Some port, _ -> port
  in
  match (Happy.connect_endpoint happy_eyeballs host [ port ], tls_config) with
  | Ok ((ipaddr, port), file_descr), None ->
      Log.debug (fun m -> m "connected to %a:%d" Ipaddr.pp ipaddr port);
      Ok (`Tcp file_descr)
  | Ok ((ipaddr, port), file_descr), Some tls_config ->
      Log.debug (fun m ->
          m "connect to %a:%d and start to upgrade to tls" Ipaddr.pp ipaddr port);
      let ( >>= ) = Result.bind in
      Http_miou_unix.to_tls tls_config file_descr
      |> Result.map_error (fun err -> `Tls err)
      >>= fun file_descr -> Ok (`Tls file_descr)
  | (Error _ as err), _ -> err

let single_request ~happy_eyeballs ?http_config tls_config ~meth ~headers ?body
    uri f acc =
  let ( let* ) = Result.bind in
  let ( let+ ) x f = Result.map f x in
  let* tls, scheme, user_pass, host, port, path = decode_uri uri in
  let* tls_config =
    if tls then
      let+ tls_config = tls_config in
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
  let* flow = connect ?port ?tls_config ~happy_eyeballs host in
  match (alpn_protocol flow, http_config) with
  | (Some `HTTP_1_1 | None), Some (`HTTP_1_1 config) ->
      single_http_1_1_request ~config flow user_pass host meth path headers body
        f acc
  | (Some `HTTP_1_1 | None), None ->
      single_http_1_1_request flow user_pass host meth path headers body f acc
  | (Some `H2 | None), Some (`H2 config) ->
      single_h2_request ~config flow scheme user_pass host meth path headers
        body f acc
  | Some `H2, None ->
      single_h2_request flow scheme user_pass host meth path headers body f acc
  | _ -> assert false

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

let request ?config ?tls_config ?authenticator ?(meth = `GET) ?(headers = [])
    ?body ?(max_redirect = 5) ?(follow_redirect = true) ~resolver:happy_eyeballs
    ~f ~uri acc =
  let tls_config =
    match tls_config with
    | Some cfg -> Ok (`Custom cfg)
    | None ->
        let alpn_protocols =
          match config with
          | None -> [ "h2"; "http/1.1" ]
          | Some (`H2 _) -> [ "h2" ]
          | Some (`HTTP_1_1 _) -> [ "http/1.1" ]
        and authenticator =
          match authenticator with
          | None -> Ca_certs.authenticator ()
          | Some authenticator -> Ok authenticator
        in
        Result.map
          (fun authenticator ->
            `Default (Tls.Config.client ~alpn_protocols ~authenticator ()))
          authenticator
  in
  let http_config =
    match config with
    | Some (`H2 cfg) -> Some (`V2 cfg)
    | Some (`HTTP_1_1 cfg) -> Some (`V1 cfg)
    | None -> None
  in
  if not follow_redirect then
    single_request ~happy_eyeballs ?http_config tls_config ~meth ~headers ?body
      uri f acc
  else
    let ( >>= ) = Result.bind in
    let rec follow_redirect count uri =
      if count = 0 then Error (`Msg "Redirect limit exceeded")
      else
        match
          single_request ~happy_eyeballs ?http_config tls_config ~meth ~headers
            ?body uri f acc
        with
        | Error _ as err -> err
        | Ok (resp, body) ->
            if Status.is_redirection resp.status then
              match Headers.get resp.headers "location" with
              | Some location ->
                  resolve_location ~uri ~location >>= fun uri ->
                  follow_redirect (pred count) uri
              | None -> Ok (resp, body)
            else Ok (resp, body)
    in
    follow_redirect max_redirect uri

module Server = Http_miou_server
