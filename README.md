# A simple HTTP client/server (HTTP/1.1 & h2) with [Miou][miou]

`httpcats` (HTTP + cats because [miou][miou]) is an implementation of an HTTP
client and server (HTTP/1.1 & h2) in pure OCaml. This implementation is based on
the [miou][miou] scheduler, [ocaml-dns][ocaml-dns] (for domain name resolution),
[happy-eyeballs][happy-eyeballs] (to manage connections), [ocaml-tls][ocaml-tls]
(for TLS protocol) & [mirage-crypto][mirage-crypto] (for cryptography),
[ca-certs][ca-certs] to obtain system certificates and [h1][h1] and [h2][h2] to
implement HTTP protocols. In all, `httpcats` requires 58 packages (including
`dune` & `ocamlfind`) for a single installation.

**U**: That's a lot of packages!

That's what's needed to end up with a pure OCaml http client. `curl`, for
example, has 13 dependencies and also contains implementations such as ftp or
smtp that are not related to an http client. A comparison would therefore be
difficult, you just have to choose your poison (OCaml or C?).

**U**: However, there are other implementations of HTTP client & server in
OCaml. Why implement it yet again?

These implementations don't use [miou], however. What's more, since
[http-lwt-client], we're opposed to the (ultimately complex) feature of being
able to choose the TLS implementation (although we understand the constraints
some users may have in wanting to use OpenSSL) and prefer to offer an HTTP
client that uses strictly [ocaml-tls][ocaml-tls]. Finally, we also want to have
control over domain resolution, rather than having to use the system's resolver.

<hr />

<tag id="fn1">**1**</tag>: Here, we point the finger at the software components
[Conduit][conduit] and [Gluten][gluten], which perform dynamic and/or static
dispatching of TLS layer implementations that we do not find suitable.

**U**: So how does `httpcats` work?

You need to initialize the random number generator required by `mirage-crypto`
and `ocaml-tls` and make your request like this:
```ocaml
let fn _meta _req _resp () = function
  | Some str -> print_string str
  | None -> ()

let () = Miou_unix.run @@ fun () ->
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  ignore (Httpcats.request ~fn ~uri:"https://robur.coop/" ());
  Mirage_crypto_rng_miou_unix.kill rng
```

It's quite... simple. You can, of course, make `POST` requests, consume the
response body in a more complex way (store it in a buffer, for example), process
the received response and lots of other things like:
- forcing the use of a version of the HTTP protocol
- define your own TLS configuration
- accept certain certificates (such as self-signed ones)
- follow or not follow redirects
- resolve domain names via `happy-eyeballs` & `ocaml-dns`

**U**: What about the server?

You can also have an HTTP/1.1 and h2 server (with TLS and a certificate you can
handle with [x509][x509]). As an example, here's a simple HTTP/1.1 server:
```ocaml
let text = "Hello World!"

let[@warning "-8"] handler _ (`V1 reqd : [ `V1 of H1.Reqd.t | `V2 of H2.Reqd.t ]) =
  let open H1 in
  let request = Reqd.request reqd in
  match request.Request.target with
  | "" | "/" | "/index.html" ->
      let headers =
        Headers.of_list
          [
            ("content-type", "text/plain; charset=utf-8")
          ; ("content-length", string_of_int (String.length text))
          ]
      in
      let resp = Response.create ~headers `OK in
      let body = Reqd.request_body reqd in
      Body.Reader.close body;
      Reqd.respond_with_string reqd resp text
  | _ ->
      let headers = Headers.of_list [ ("content-length", "0") ] in
      let resp = Response.create ~headers `Not_found in
      Reqd.respond_with_string reqd resp ""

let server sockaddr = Httpcats.Server.clear ~handler sockaddr

let () =
  let sockaddr = Unix.(ADDR_INET (inet_addr_loopback, 8080)) in
  Miou_unix.run @@ fun () ->
  let domains = Miou.Domain.available () in
  let prm = Miou.async @@ fun () -> server sockaddr in
  if domains > 0
  then Miou.parallel server (List.init domains (Fun.const sockaddr))
       |> List.iter (function Ok () -> () | Error exn -> raise exn);
  Miou.await_exn prm
```

Again, it's pretty straightforward. This server takes the opportunity to use all
your cores thanks to [miou][miou]. You can also run the program with a specific
number of domains:
```sh
$ ocamlfind opt -linkpkg -package digestif.c,httpcats server.ml
$ MIOU_DOMAINS=2 ./a.out
```

## Benchmarks

Some contributors to the OCaml community wanted to benchmark different HTTP
implementations in OCaml. You can find more details [here][discuss-benchmark].
As for `httpcats`, a benchmark was developed and proposed
[here][FrameworkBenchmarks].

This benchmark tool has the advantage of being fairly reproducible. Here are
the results between `httpun+eio` and `httpcats` (`h1+miou`) (on AMD Ryzen 9
7950X 16-Core):

### `httpcats` (or `h1` + `miou`)

| clients | threads | latencyAvg | latencyMax | latencyStdev | totalRequests |
|---------|---------|------------|------------|--------------|---------------|
| 16      | 16      | 47.43us    | 2.27ms     | 38.48us      | 5303700       |
| 32      | 32      | 71.73us    | 1.04ms     | 47.58us      | 7016729       |
| 64      | 32      | 140.29us   | 5.72ms     | 121.50us     | 7658146       |
| 128     | 32      | 279.73us   | 11.35ms    | 287.92us     | 7977306       |
| 256     | 32      | 519.02us   | 16.89ms    | 330.20us     | 7816435       |
| 512     | 32      | 1.06ms     | 37.42ms    | 534.14us     | 7409781       |

### `httpun` & `eio`

| clients | threads | latencyAvg | latencyMax | latencyStdev | totalRequests |
|---------|---------|------------|------------|--------------|---------------|
| 16      | 16      | 1.19ms     | 17.12ms    | 2.09ms       | 2966727       |
| 32      | 32      | 0.91ms     | 17.49ms    | 1.65ms       | 5366296       |
| 64      | 32      | 1.08ms     | 17.30ms    | 1.82ms       | 5919733       |
| 128     | 32      | 1.16ms     | 18.62ms    | 1.76ms       | 6187300       |
| 256     | 32      | 1.41ms     | 26.61ms    | 1.96ms       | 6604454       |
| 512     | 32      | 1.84ms     | 32.37ms    | 2.23ms       | 6798222       |

### Interpretations

As we can see, `httpcats` performs **better** than `eio` (with `httpun`) in
terms of latency and the number of requests it can handle per second.

To be precise, `h1` and `httpun` are both forks of `httpaf` and the code is
very similar. If we had to explain a difference between these two benchmarks,
it would **not** be due to `h1` or `httpun`.

CoHTTP is not included in this benchmark because it is more a comparison
between schedulers than implementations of the HTTP/1.1 protocol. In this case,
`h1` and `httpun`, due to their similarities with `httpaf`, normally perform
better than CoHTTP — you can see [the conference][httpaf-conf] about `httpaf`
or the official [repository][httpaf]. These implementations also allow for
support of the [`h2`][h2] protocol (which is not currently possible with
CoHTTP).

The real difference lies between `miou` and `eio` and their task management
policies. For more details, please refer to [the Miou documentation][miou-doc]:
overall, Miou offers more _poll points_ than Eio, which provides more
opportunities to manage more clients. This is one of Miou's stated objectives:
to be a scheduler designed for this type of service.

<hr />

<tag id="fn2">**2**</tag>: In the discussion thread presented above, there is
also mention of `httpaf+lwt`, which performs even better than `httpcats`. It is
specified that the use of domains in this benchmark is **not** safe.

<tag id="fn3">**3**</tag>: It should be noted that `eio` uses
[`io_uring`][io_uring] while Miou uses `select(3P)`. It is possible to improve
Miou to use `epoll(7)` or `io_uring` (and make sure that `httpcats` uses this
implementation) but, as it stands, `select()` is sufficient.

[miou]: https://github.com/robur-coop/miou
[ocaml-dns]: https://github.com/mirage/ocaml-dns
[happy-eyeballs]: https://github.com/robur-coop/happy-eyeballs
[ocaml-tls]: https://github.com/mirleft/ocaml-tls
[ca-certs]: https://github.com/mirage/ca-certs
[h1]: https://github.com/robur-coop/ocaml-h1
[h2]: https://github.com/anmonteiro/ocaml-h2
[http-lwt-client]: https://github.com/robur-coop/http-lwt-client
[x509]: https://github.com/mirleft/ocaml-x509
[io_uring]: https://github.com/ocaml-multicore/ocaml-uring
[nginx-benchmark]: https://openbenchmarking.org/test/pts/nginx&eval=f9e860ca197d88a133e3ae0496e96fa3c79e33fe#metrics
[cohttp]: https://github.com/mirage/ocaml-cohttp
[rewrk]: https://github.com/lnx-search/rewrk
[mirage-crypto]: https://github.com/mirage/mirage-crypto
[miou-doc]: https://docs.osau.re/miou/
[httpaf-conf]: https://watch.ocaml.org/w/b2KP5hMngXkyVU3z7yNLpG
[httpaf]: https://github.com/inhabitedtype/httpaf
[discuss-benchmark]: https://discuss.ocaml.org/t/lwt-multi-processing-much-more-performant-than-eio-multi-core/16395
[FrameWorkBenchmarks]: https://github.com/TechEmpower/FrameworkBenchmarks/pull/10009
[conduit]: https://github.com/mirage/ocaml-conduit
[gluten]: https://github.com/anmonteiro/gluten
