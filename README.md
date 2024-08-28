# A simple HTTP client/server (http/1.1 & h2) with [Miou][miou]

httpcats (http + cats because [miou][miou]) is an implementation of an http
client and server (http/1.1 & h2) in pure OCaml. This implementation is based on
the [miou][miou] scheduler, [ocaml-dns][ocaml-dns] (for domain name resolution),
[happy-eyeballs][happy-eyeballs] (to manage connections), [ocaml-tls][ocaml-tls]
(for TLS protocol) & [mirage-crypto][mirage-crypto] (for cryptography),
[ca-certs][ca-certs] to obtain system certificates and [h1][h1] and [h2][h2] to
implement http protocols. In all, httpcats requires 58 packages (including
`dune` & `ocamlfind`) for a single installation.

That's a lot of packages!

That's what's needed to end up with a pure OCaml http client. `curl`, for
example, has 13 dependencies and also contains implementations such as ftp or
smtp that are not related to an http client. A comparison would therefore be
difficult, you just have to choose your poison (OCaml or C?).

However, there are other implementations of http client & server in OCaml.

These implementations don't use [miou], however. What's more, since
[http-lwt-client], we're opposed to the (ultimately complex) feature of being
able to choose the TLS implementation (although we understand the constraints
some users may have in wanting to use OpenSSL) and prefer to offer an http
client that uses strictly [ocaml-tls][ocaml-tls]. Finally, we also want to have
control over domain resolution, rather than having to use the system's resolver.

So how does httpcats work?

You need to initialize the random number generator required by `mirage-crypto`
and `ocaml-tls` and make your request like this:
```ocaml
let f _meta _resp () = function
  | Some str -> print_string str
  | None -> ()

let () = Miou_unix.run @@ fun () ->
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  ignore (Httpcats.request ~f ~uri:"https://robur.coop/" ());
  Mirage_crypto_rng_miou_unix.kill rng
```

It's quite... simple. You can, of course, make `POST` requests, consume the
response body in a more complex way (store it in a buffer, for example), process
the received response and lots of other things like:
- forcing the use of a version of the http protocol
- define your own tls configuration
- accept certain certificates (such as self-signed ones)
- follow or not follow redirects
- resolve domain names via `happy-eyeballs`

What about the server?

You can also have an http/1.1 and h2 server (with tls and a certificate you can
handle with [x509][x509]). As an example, here's a simple http/1.1 server:
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

And what about performance?

httpcats & miou essentially want to take advantage of the domains available.
Here are the results of a benchmark of `examples/server.ml` with [rewrk][rewrk]:

| threads (server) | threads (client) | connections / threads | requests/s |
|------------------|------------------|-----------------------|------------|
| 0                | 12               | 256                   | ~ 45k      |
| 1                | 12               | 256                   | ~ 70k      |
| 2                | 12               | 256                   | ~ 72k      |
| 4                | 12               | 256                   | ~ 130k     |
| 8                | 12               | 256                   | ~ 160k     |
| MIOU_DOMAINS     | rewrk -t         | rewrk -c              |            |

More domains than 8 won't handle more requests. It seems that 8 domains is the
glass ceiling for httpcats. For comparison, here's the result for
[CoHTTP][cohttp] (which, this time, takes advantage of [io_uring][io_uring]):

| threads (server) | threads (client) | connections / threads | requests/s |
|------------------|------------------|-----------------------|------------|
|                  | 12               | 256                   | ~ 110k     |

Just like the choice between a pure http implementation in OCaml or curl, choose
your poison! However, there are several things to note about this benchmark:
1) `miou.unix` uses `select()` and our benchmarks clearly show that the
   bottleneck concerns this system call. CoHTTP uses `io_uring`, which saves
   passage between user space and the kernel. In this respect, there is clearly
   an area for optimization for miou (`io_uring`) to avoid this bottleneck.
2) CoHTTP uses a single domain to manage your requests. For the sake of
   comparison and fair play, we should compare CoHTTP and httpcats with
   `MIOU_DOMAINS=0`.
3) The request handling behavior of httpcats and CoHTTP may also differ.
   httpcats can also handle the h2 protocol (which CoHTTP doesn't).
4) The benchmark does not concern the TLS layer and, as mentioned above, CoHTTP
   may, depending on your system, use OpenSSL where httpcats will only use
   `ocaml-tls`.

All this to say that these results should certainly be taken with a grain of
salt. What can really be concluded about httpcats is its ability to stand
shoulder to shoulder with other (C-based) server implementations - for example,
on AMD Ryzen 9 7950X 16-Core, nginx can handle [178k req/s][nginx-benchmark].

Finally, benchmarks (especially those concerning http) are difficult to make
because they are hard to reproduce. So take the results as they come, but don't
say that httpcats is faster than <any-dumb-http-implementation>.

Finally, wouldn't this be the best http stack in OCaml?

It is for me, of course, but it may not be for you. The best way to find out is
to test and compare according to your objectives (which are, of course,
different from mine). The project is also experimental at this stage -
production use could break the internet - which could be interesting. Just like
miou, httpcats allows for a diversity of implementations and offers a choice
that, while requiring knowledge and real reflection on the whys and wherefores,
brings freedom because we're no longer locked into using just one and unique
implementation!

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
