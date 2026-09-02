exception Closed_by_peer
(* NOTE(dinosaure): it may happen that it is impossible to [write] to a peer.
   The standard error is [EPIPE] as well as a [SIGPIPE] signal that we ignore
   (at the application level). The user must transform this error by raising the
   [Closed_by_peer] exception. In this way, the "Runtime" is informed that the
   connection has been closed. *)

(* NOTE(dinosaure): the HTTP state-machines (and [Faraday]) speak
   {!type:Bstr.t}, so this is what we require from a flow too. A stack which is
   able to fill and consume a bigstring directly then needs {b no} intermediate
   buffer at all: what the kernel (or [utcp]) writes is what the parser reads.

   A stack which can only speak [bytes]/[string] (typically [ocaml-tls], whose
   plaintext lives in a [string]) implements {!module-type:BYTES} instead and
   is adapted with {!module:Of_bytes}, which owns the scratch buffer needed to
   bridge the two. The buffer then belongs to the flow that actually needs it
   rather than to the [Runtime]. *)
module type S = sig
  type t

  val read : t -> Bstr.t -> off:int -> len:int -> int
  (** [read flow bstr ~off ~len] reads at most [len] bytes from [flow] into
      [bstr] at [off] and returns how many bytes were actually read. [0] means
      end-of-input. *)

  val writev : t -> Bstr.t Faraday.iovec list -> unit
  (** [writev flow iovecs] writes all the given [iovecs] to [flow].

      {b NOTE}: the buffers of [iovecs] belong to the caller and are reused as
      soon as [writev] returns. An implementation which defers the actual
      transmission (such as [utcp], which keeps the segment in its own state)
      must take a copy.

      @raise Closed_by_peer if the peer closed the connection on its side. *)

  val close : t -> unit
  val shutdown : t -> [ `read | `write | `read_write ] -> unit
end

(** A flow which can only speak [bytes]/[string]. Use {!module:Of_bytes} to
    obtain an {!module-type:S} out of it. *)
module type BYTES = sig
  type t

  val read : t -> ?off:int -> ?len:int -> bytes -> int
  val write : t -> ?off:int -> ?len:int -> string -> unit
  val close : t -> unit
  val shutdown : t -> [ `read | `write | `read_write ] -> unit
end

let default_chunk_size = 0x10000
(* NOTE(dinosaure): the largest amount we ask the underlying flow for on a
   single [read], and the largest slice of a [Faraday] iovec we [write] in one
   go. Such a buffer lands in the major heap (a [Bytes.create] stays in the
   minor heap only up to 2047 bytes on a 64-bit machine: [Max_young_wosize] is
   256 words, so [Bytes.create 2047] costs 257 minor words and
   [Bytes.create 2048] costs none), which is what we want: it is allocated once
   per connection and lives as long as it does, so we would rather not have it
   travel through the minor heap at all.

   HISTORY(dinosaure): this constant lived in the [Runtime] as
   [(Sys.word_size / 8 * 256) - 1] - precisely that 2047 bound - because a
   scratch buffer was then allocated on *every* [read], so keeping it young
   mattered. It later became [16384 - 1], which kept the [- 1] although the
   buffers had become per-connection and the bound meaningless. Down here an
   off-by-one below a power of two is in fact harmful: a TLS record carries at
   most [16384] bytes of plaintext, so asking for [16383] splits a full record
   into a [16383] read followed by a [1] byte one. *)

(** The result of {!module:Of_bytes}: a bigstring flow which owns the scratch
    buffer needed to talk to the underlying [bytes]/[string] one. *)
module type CHANNEL = sig
  include S

  type flow

  val make : ?chunk_size:int -> flow -> t
  val prj : t -> flow
end

module Of_bytes (Flow : BYTES) : CHANNEL with type flow = Flow.t = struct
  type flow = Flow.t
  type t = { flow: Flow.t; mutable rd: bytes; mutable wr: bytes; chunk: int }

  (* NOTE(dinosaure): the scratch buffers grow on demand (up to [chunk]) rather
     than being allocated at [chunk] straight away. A connection which only
     exchanges small messages keeps small buffers, while a connection streaming
     a large body converges to [chunk] after a couple of allocations. *)
  let make ?(chunk_size = default_chunk_size) flow =
    { flow; rd= Bytes.empty; wr= Bytes.empty; chunk= chunk_size }

  let prj { flow; _ } = flow

  let[@inline] reserve buf len =
    if Bytes.length buf >= len then buf else Bytes.create len

  let read t bstr ~off:dst_off ~len =
    let len = Int.min len t.chunk in
    t.rd <- reserve t.rd len;
    let len' = Flow.read t.flow t.rd ~off:0 ~len in
    Bstr.blit_from_bytes t.rd ~src_off:0 bstr ~dst_off ~len:len';
    len'

  let writev t bstrs =
    let fn { Faraday.buffer; off; len } =
      let rec go src_off len =
        if len > 0 then begin
          let n = Int.min len t.chunk in
          t.wr <- reserve t.wr n;
          Bstr.blit_to_bytes buffer ~src_off t.wr ~dst_off:0 ~len:n;
          Flow.write t.flow ~off:0 ~len:n (Bytes.unsafe_to_string t.wr);
          go (src_off + n) (len - n)
        end
      in
      go off len
    in
    List.iter fn bstrs

  let close t = Flow.close t.flow
  let shutdown t cmd = Flow.shutdown t.flow cmd
end
