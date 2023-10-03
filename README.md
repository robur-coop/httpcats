# A simple HTTP client (http/1.1 & h2) with [Miou][miou]

```ocaml
let () =
  Miou_unix.run @@ fun () ->
  let daemon, resolver = Happy.stack () in (* happy-eyeballs *)
  let dns = Dns_miou.create resolve in
  Happy.inject_resolver ~getaddrinfo:(getaddrinfo dns) resolver; (* dns x happy-eyeballs *)
  let acc = Buffer.create 0x100 in
  let f buf str = Buffer.add_string buf str; buf in
  begin match Httpcats.request ~resolver ~f ~uri:"https://blog.osau.re/" acc with
    | Ok (_response, buf) ->
      Format.printf "%s%!" (Buffer.contents buf)
    | Error err ->
      Format.eprintf "Got an error: %a\n%!" Httpcats.pp_error err
  end;
  Happy.kill daemon
```

**NOTE**: it requires the upstream version of `miou`!

- [ ] Fix the issue between HTTP/1.1 and TLS (and close-notify)
- [ ] Implement some tests
- [ ] Documentation (.ml & .mli)
- [ ] DNS resolution over UDP
- [ ] DNS over TLS

[miou]: https://github.com/robur-coop/miou
