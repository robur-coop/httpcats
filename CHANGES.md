# v0.2.1 (2026-03-25) Paris - France

- Use `Miou.Ownership` to release correctly our resource in any cases (including
  the cancellation case) (@dinosaure, #47)
- Replace `assert false` by `failwith` (@dinosaure, #47)
- Improve performances (@dinosaure, #46)
  We use a pre-allocated buffer to read/write things into the given flow now
  We don't try to split bigarray to small strings
  We fixed a bit the way to stop a server

  A benchmark was made with `wrk` and compare `httpcats`, `httpun+eio`, `vif`
  and `nginx`. It is available here: https://robur-coop.github.io/httpcats/
  The protocol of the benchmark is described into our file `bench/PROTOCOL.md`.
  The collected results from our machine is available into our file
  `bench/BENCH.md`. Implementations are available into the `bench/` folder.

# v0.2.0 (2026-02-19) Paris - France

- Improve the OPAM file and the README.md (@geazi-anc, #39)
- Split out the core of `httpcats` to be re-usable by unikernels (@dinosaure, #40)
- Avoid a memory leak about logs (@dinosaure, #42)
- Upgrade GitHub CI and ocamlformat (@dinosaure, #43)

# v0.1.0 (2025-08-09) Paris - France

- Add the support of websocket (@swrup & @dinosaure)
- Improve the `Httpcats.request` and handle cookies and redirections well
  (@dinosaure)
- Improve the runtime implement to process an HTTP communication (with http/1.1
  & h2) (@dinosaure)
- Improve the documentation (@dinosaure)

This changelog does not contain PRs referring to additions to httpcats, as they
were produced with a quick iteration to finalise the release of httpcats.0.1.0.

# v0.0.1 (2024-09-13) Paris - France

- First release
