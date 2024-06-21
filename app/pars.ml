let anchor = Unix.gettimeofday ()
let () = Printexc.record_backtrace true

let reporter ppf =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("[%a]%a[%a][%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Cyan (fmt "%.04f"))
        (Unix.gettimeofday () -. anchor)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Blue int)
        (Stdlib.Domain.self () :> int)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stderr)

(* let () = Logs.set_level ~all:true (Some Logs.Debug) *)
let () = Logs_threaded.enable ()
let () = Printexc.record_backtrace true

let colors =
  (* import matplotlib.cm
     for i in matplotlib.cm.rainbow(numpy.linspace(0.2, 1, 20)):
       print(matplotlib.colors.rgb2hex(i))
  *)
  Array.map Progress.Color.hex
    [| "#1996f3"; "#06aeed"; "#10c6e6"; "#27dade"; "#3dead5"
     ; "#52f5cb"; "#66fcc2"; "#7dffb6"; "#92fda9"; "#a8f79c"
     ; "#bced8f"; "#d2de81"; "#e8cb72"; "#feb562"; "#ff9b52"
     ; "#ff8143"; "#ff6232"; "#ff4121"
    |]
[@@ocamlformat "disable"]

let pick_colour =
  let count = ref (-1) in
  fun () ->
    count := succ !count mod Array.length colors;
    colors.(!count)

let out = Mutex.create ()
let progress = Mutex.create ()

let with_lock ~lock fn =
  Mutex.lock lock;
  let finally () = Mutex.unlock lock in
  Fun.protect ~finally fn

let epr fmt =
  let finally () = Mutex.unlock out in
  Mutex.lock out;
  Fun.protect ~finally @@ fun () -> Format.eprintf fmt

type reporter = All_knowning of int Progress.Reporter.t | Unknown of int

let get_length { Httpcats.headers; _ } =
  let headers = H2.Headers.to_list headers in
  let headers =
    List.map (fun (k, v) -> (String.lowercase_ascii k, v)) headers
  in
  Option.map int_of_string (List.assoc_opt "content-length" headers)

let download ~orphans ~events ~uid ~resolver ~uri =
  let _prm =
    Miou.call ~orphans @@ fun () ->
    let got_response = ref false in
    let counter = ref 0 in
    let[@warning "-8"] (Ok (_, _, _, _, _, path)) = Httpcats.decode_uri uri in
    let f resp () str =
      if not !got_response then begin
        Miou.Queue.enqueue events
          (`Response (uid, Filename.basename path, resp));
        Logs.debug (fun m ->
            m "response for %s: %a" uri Httpcats.pp_response resp);
        got_response := true
      end;
      let max = Option.value ~default:0 (get_length resp) in
      counter := !counter + String.length str;
      Logs.debug (fun m ->
          m "got %d/%d byte(s) from %d:%s" !counter max uid uri);
      Miou.Queue.enqueue events (`Data (uid, String.length str))
    in
    match Httpcats.request ~resolver ~uri ~f () with
    | Ok (_response, ()) ->
        Logs.debug (fun m -> m "%d:%s downloaded" uid uri);
        Miou.Queue.enqueue events (`End uid)
    | Error err -> Miou.Queue.enqueue events (`Error (uid, err))
  in
  ()

type event =
  [ `Response of int * string * Httpcats.response
  | `Data of int * int
  | `End of int
  | `Error of int * Httpcats.error ]

type display = (unit, unit) Progress.Display.t

type t = {
    gen: int Atomic.t
  ; orphans: unit Miou.orphans
  ; events: event Miou.Queue.t
  ; reporters: reporter array
  ; display: display
  ; align: int
  ; resolver: Happy_eyeballs_miou_unix.happy
}

let make ~resolver ~filenames =
  let gen = Atomic.make 0 in
  let orphans = Miou.orphans () in
  let events = Miou.Queue.create () in
  let reporters = Array.make (List.length filenames) (Unknown 0) in
  let display =
    let make_formatter oc =
      Format.make_formatter (output_substring oc) (fun () -> flush oc)
    in
    let config = Progress.Config.v ~ppf:(make_formatter stdout) () in
    Progress.Display.start ~config Progress.Multi.(blank)
  in
  let align =
    List.fold_left
      (fun acc filename -> max acc (String.length filename))
      0 filenames
  in
  let [] = Progress.Display.reporters display in
  { gen; orphans; events; reporters; display; align; resolver }

let bar t ~filename ~response =
  match get_length response with
  | Some total ->
      let open Progress.Line in
      let style =
        Bar_style.v ~delims:("|", "|")
          [ "█"; "▉"; "▊"; "▋"; "▌"; "▍"; "▎"; "▏"; " " ]
      in
      let bar = bar ~color:(pick_colour ()) ~style:(`Custom style) total in
      list [ lpad t.align (const filename); bar; bytes ]
  | None ->
      let open Progress.Line in
      let spin =
        spinner ~frames:[ "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" ] ()
      in
      list [ lpad t.align (const filename); spin; bytes; bytes_per_sec ]

let consume t events =
  let handle = function
    | `Response (uid, filename, response) ->
        let downloaded =
          match t.reporters.(uid) with
          | Unknown downloaded -> downloaded
          | _ -> assert false (* XXX(dinosaure): impossible! *)
        in
        let reporter =
          Progress.Display.add_line ~above:1 t.display
            (bar t ~filename ~response)
        in
        Progress.Reporter.report reporter downloaded;
        t.reporters.(uid) <- All_knowning reporter
    | `Data (uid, downloaded) -> (
        match t.reporters.(uid) with
        | Unknown _old -> t.reporters.(uid) <- Unknown downloaded
        | All_knowning reporter -> Progress.Reporter.report reporter downloaded)
    | `End uid -> (
        match t.reporters.(uid) with
        | All_knowning reporter -> Progress.Reporter.finalise reporter
        | _ -> ())
    | `Error (uid, err) ->
        Progress.interject_with (fun () ->
            Fmt.pr "[%a]: %a\n%!"
              Fmt.(styled `Red string)
              "ERROR" Httpcats.pp_error err);
        Logs.err (fun m ->
            m "Got an error for (%04d): %a" uid Httpcats.pp_error err)
  in
  List.iter handle events

let rec run t uris () =
  Progress.Display.tick t.display;
  match (Miou.care t.orphans, uris) with
  | None, [] ->
      let events' = Miou.Queue.(to_list (transfer t.events)) in
      consume t events';
      Progress.Display.finalise t.display
  | None, uri :: rest ->
      download ~orphans:t.orphans ~events:t.events
        ~uid:(Atomic.fetch_and_add t.gen 1)
        ~resolver:t.resolver ~uri;
      let events' = Miou.Queue.(to_list (transfer t.events)) in
      consume t events';
      run t rest (Miou.yield ())
  | Some prm, uri :: rest ->
      Option.iter Miou.await_exn prm;
      download ~orphans:t.orphans ~events:t.events
        ~uid:(Atomic.fetch_and_add t.gen 1)
        ~resolver:t.resolver ~uri;
      let events' = Miou.Queue.(to_list (transfer t.events)) in
      consume t events';
      run t rest (Miou.yield ())
  | Some prm, [] ->
      Option.iter Miou.await_exn prm;
      let events' = Miou.Queue.(to_list (transfer t.events)) in
      consume t events';
      run t [] (Miou.yield ())

let get_uris_from_stdin () =
  let rec go acc =
    match input_line stdin with
    | exception End_of_file -> List.rev acc
    | line -> go (line :: acc)
  in
  go []

let getaddrinfo dns =
  {
    Happy_eyeballs_miou_unix.getaddrinfo=
      (fun record host -> Dns_client_miou_unix.getaddrinfo dns record host)
  }

let sigpipe = 13
let () = Sys.set_signal sigpipe Sys.Signal_ignore
let () = Printexc.record_backtrace true
let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)

let () =
  Miou_unix.run @@ fun () ->
  let uris = get_uris_from_stdin () in
  let uris =
    List.filter_map
      (fun uri ->
        match Httpcats.decode_uri uri with Ok _ -> Some uri | Error _ -> None)
      uris
  in
  let filenames =
    List.map
      (fun uri ->
        let[@warning "-8"] (Ok (_, _, _, _, _, path)) =
          Httpcats.decode_uri uri
        in
        Filename.basename path)
      uris
  in
  Logs.debug (fun m -> m "Got %d uri(s)" (List.length uris));
  let daemon, resolver = Happy_eyeballs_miou_unix.make () in
  let nameservers =
    (`Udp, [ `Plaintext (Ipaddr.of_string_exn "8.8.8.8", 53) ])
  in
  let dns = Dns_client_miou_unix.create ~nameservers resolver in
  Happy_eyeballs_miou_unix.inject_resolver ~getaddrinfo:(getaddrinfo dns)
    resolver;
  let t = make ~resolver ~filenames in
  let prm = Miou.call_cc (run t uris) in
  let result = Miou.await prm in
  Happy_eyeballs_miou_unix.kill daemon;
  match result with Ok () -> () | Error exn -> raise exn
