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

let _plaintext req _server () =
  let open Vif.Response.Syntax in
  let str = "Hello, World!" in
  let field = "date" in
  let* () = Vif.Response.add ~field (date ()) in
  let* () = Vif.Response.with_text req str in
  Vif.Response.respond `OK

let _json req _server () =
  let open Vif.Response.Syntax in
  let json =
    let open Jsont in
    let message = ("message", Meta.none)
    and hello_world = String ("Hello, World!", Meta.none) in
    Jsont.(Object ([ (message, hello_world) ], Meta.none))
  in
  let field = "date" in
  let* () = Vif.Response.add ~field (date ()) in
  let* () = Vif.Response.with_json req Jsont.json json in
  Vif.Response.respond `OK

let _not_found req _target _server () =
  let process =
    let open Vif.Response.Syntax in
    let* () = Vif.Response.with_text req "m00." in
    Vif.Response.respond `OK
  in
  Some process

let routes =
  let open Vif.Uri in
  let open Vif.Route in
  [
    get (rel / "plaintext" /?? nil) --> _plaintext
  ; get (rel / "json" /?? nil) --> _json
  ]

let () =
  let domains =
    match Sys.getenv_opt "DOMAINS" with
    | Some value -> int_of_string_opt value
    | None -> None
  in
  Miou_unix.run ?domains @@ fun () ->
  let sockaddr = Unix.(ADDR_INET (inet_addr_loopback, 8080)) in
  let handlers = [ _not_found ] in
  let cfg = Vif.config ~backlog:4096 sockaddr in
  Vif.run ~cfg ~handlers routes ()
