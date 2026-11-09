let pp_uri ppf (is_tls, scheme, user_pass, host, port, path) =
  let pp_user_pass ppf (user, pass) =
    Fmt.pf ppf "(@[<1>%S,@ @[%a@]@])" user Fmt.(Dump.option string) pass
  in
  Fmt.pf ppf "(@[<1>%b,@ %S,@ @[<hov>%a@],@ %S,@ @[<hov>%a@],@ %S@])" is_tls
    scheme
    Fmt.(Dump.option pp_user_pass)
    user_pass host
    Fmt.(Dump.option int)
    port path

let uri = Alcotest.testable pp_uri ( = )
let msg = Alcotest.testable (fun ppf (`Msg msg) -> Fmt.string ppf msg) ( = )
let value = Alcotest.result uri msg

let test00 =
  Alcotest.test_case "simple" `Quick @@ fun () ->
  let uri = (false, "", None, "example.org", None, "/") in
  Alcotest.(check value)
    "example.org"
    (Httpcats.decode_uri "example.org")
    (Ok uri);
  let uri = (false, "", None, "", Some 3000, "/") in
  Alcotest.(check value) ":3000" (Httpcats.decode_uri ":3000") (Ok uri);
  let uri = (false, "", None, "", None, "/foo") in
  Alcotest.(check value) ":/foo" (Httpcats.decode_uri ":/foo") (Ok uri)

let test01 =
  Alcotest.test_case "location" `Quick @@ fun () ->
  let uri = "https://user:pass@builds.robur.coop/old-path" in
  let location = "//builds.osau.re/new-path" in
  Alcotest.(check (result string reject))
    "schema relative resolve_location" (Ok "https://builds.osau.re/new-path")
    (Httpcats.resolve_location ~uri ~location);
  let uri = "https://user:pass@builds.robur.coop/job/mollymawk/build/latest"
  and location = "/job/mollymawk/build/7aba1606-06e6-4e66-9d3b-8410f0183a49" in
  Alcotest.(check (result string reject))
    "relative resolve_location"
    (Ok ("https://user:pass@builds.robur.coop" ^ location))
    (Httpcats.resolve_location ~uri ~location)

let () = Alcotest.run "decode_uri" [ ("simple", [ test00; test01 ]) ]
