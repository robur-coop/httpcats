open Httpun
open Httpun_eio
open Eio.Std

let get_date () = Unix.(gettimeofday () |> gmtime)

let dow = function
  | 0 -> "Sun"
  | 1 -> "Mon"
  | 2 -> "Tue"
  | 3 -> "Wed"
  | 4 -> "Thu"
  | 5 -> "Fri"
  | _ -> "Sat"

let month = function
  | 0 -> "Jan"
  | 1 -> "Feb"
  | 2 -> "Mar"
  | 3 -> "Apr"
  | 4 -> "May"
  | 5 -> "Jun"
  | 6 -> "Jul"
  | 7 -> "Aug"
  | 8 -> "Sep"
  | 9 -> "Oct"
  | 10 -> "Nov"
  | _ -> "Dec"

let date () =
  let d = get_date () in
  Format.sprintf "%s, %02d %s %4d %02d:%02d:%02d GMT" (dow d.tm_wday) d.tm_mday
    (month d.tm_mon) (1900 + d.tm_year) d.tm_hour d.tm_min d.tm_sec

let request_handler (_ : Eio.Net.Sockaddr.stream) {Gluten.reqd; _} =
  let req = Reqd.request reqd in
  match req.target with
  | "/json" ->
    let obj = `Assoc ["message", `String "Hello, World!"] in
    let payload = Yojson.to_string obj in
    let headers =
      Headers.of_rev_list
        [
          "content-length", string_of_int @@ String.length payload
        ; "content-type", "application/json"; "server", "httpun"
        ; "date", date ()
        ] in
    let rsp = Response.create ~headers `OK in
    Reqd.respond_with_string reqd rsp payload
  | "/plaintext" ->
    let payload = "Hello, World!" in
    let headers =
      Headers.of_rev_list
        [
          "content-length", string_of_int @@ String.length payload
        ; "content-type", "text/plain"; "server", "httpun"; "date", date ()
        ] in
    let rsp = Response.create ~headers `OK in
    Reqd.respond_with_string reqd rsp payload
  | _ ->
    let moo = "m00." in
    let headers =
      Headers.of_list ["content-length", string_of_int @@ String.length moo]
    in
    let rsp = Response.create ~headers `OK in
    Reqd.respond_with_string reqd rsp moo

let error_handler (_ : Eio.Net.Sockaddr.stream) ?request:_ error start_response
    =
  let response_body = start_response Headers.empty in
  (match error with
  | `Exn exn ->
    Body.Writer.write_string response_body (Printexc.to_string exn)
    ; Body.Writer.write_string response_body "\n"
  | #Status.standard as error ->
    Body.Writer.write_string response_body (Status.default_reason_phrase error))
  ; Body.Writer.close response_body

let () =
  let domain_count =
    match Sys.getenv_opt "DOMAINS" with
    | Some value -> int_of_string value
    | None -> Stdlib.Domain.recommended_domain_count () in
  Printf.eprintf "Detected %d cores\n" domain_count
  ; let ulimit_n =
      Unix.open_process_in "ulimit -n" |> input_line |> int_of_string in
    Printf.eprintf "Detected %d max open files\n" ulimit_n
    ; let somaxconn =
        Stdlib.open_in "/proc/sys/net/core/somaxconn"
        |> Stdlib.input_line
        |> Stdlib.int_of_string in
      Printf.eprintf "Detected %d somaxconn\n" somaxconn
      ; let backlog = Stdlib.min ulimit_n somaxconn in

        Eio_main.run @@ fun env ->
        Switch.run @@ fun sw ->
        let dm = Eio.Stdenv.domain_mgr env in
        let addr = `Tcp (Eio.Net.Ipaddr.V4.any, 8080) in
        let listening_socket =
          Eio.Net.listen ~sw env#net addr ~backlog ~reuse_addr:true in

        let connection_handler flow addr =
          (try
             Eio_unix.Fd.use_exn "setsockopt" (Eio_unix.Net.fd flow) (fun fd ->
                 Unix.setsockopt fd Unix.TCP_NODELAY true)
           with Unix.Unix_error _ -> ())
          ; Server.create_connection_handler ~request_handler ~error_handler ~sw
              addr flow in
        let run listening_socket =
          Eio.Net.run_server ~additional_domains:(dm, domain_count)
            listening_socket ~max_connections:backlog connection_handler
            ~on_error:(traceln "Error handling connection: %a" Fmt.exn) in
        run listening_socket
