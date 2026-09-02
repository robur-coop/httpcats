module TCP : Runtime.Flow.S with type t = Miou_unix.file_descr
(** A TCP flow which reads into and writes from bigstrings directly (via
    {!val:Unix.read_bigarray} and {!val:Unix.single_write_bigarray}): no
    intermediate buffer at all. *)

module TLS : Runtime.Flow.CHANNEL with type flow = Tls_miou_unix.t
(** A TLS flow. [ocaml-tls] speaks [bytes]/[string], so this one goes through
    {!module:Runtime.Flow.Of_bytes} and owns the scratch buffer that implies.
    Use {!val:Runtime.Flow.CHANNEL.make} to wrap a {!type:Tls_miou_unix.t} and
    {!val:Runtime.Flow.CHANNEL.prj} to get it back. *)
