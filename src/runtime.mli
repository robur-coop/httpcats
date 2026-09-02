module Flow = Flow

module type S = sig
  type t

  val next_read_operation : t -> [ `Read | `Yield | `Close | `Upgrade ]
  val read : t -> Bstr.t -> off:int -> len:int -> int
  val read_eof : t -> Bstr.t -> off:int -> len:int -> int
  val yield_reader : t -> (unit -> unit) -> unit

  val next_write_operation :
       t
    -> [ `Write of Bstr.t Faraday.iovec list
       | `Close of int
       | `Yield
       | `Upgrade ]

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  val yield_writer : t -> (unit -> unit) -> unit
  val report_exn : t -> exn -> unit
  val is_closed : t -> bool
end

module type CONNECTION = sig
  type conn
  type flow

  val run :
       conn
    -> ?tags:Logs.Tag.set
    -> ?read_buffer_size:int
    -> ?upgrade:(flow -> unit)
    -> flow
    -> unit Miou.t
  (** [run conn ?read_buffer_size flow] runs the given state-machine [conn] on
      top of [flow].

      [read_buffer_size] is the initial size of the bigstring where we
      accumulate what the peer sends us (it grows on demand); it should be
      [H1.Config.read_buffer_size] / [H2.Config.read_buffer_size]. *)
end

(** [Make] runs an HTTP state-machine on top of a flow. The [Runtime] itself
    allocates no scratch buffer: it hands the connection's bigstring to
    {!val:Flow.S.read} and the [Faraday] iovecs to {!val:Flow.S.writev}. A flow
    which can only speak [bytes]/[string] goes through {!module:Flow.Of_bytes},
    which owns the buffer it needs. *)
module Make (Flow : Flow.S) (Runtime : S) :
  CONNECTION with type conn = Runtime.t and type flow = Flow.t

val terminate : unit Miou.orphans -> unit
val clean : unit Miou.orphans -> unit
