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

module Make (Flow : Flow.S) (Runtime : S) : sig
  type conn = Runtime.t
  type flow = Flow.t

  val run :
       conn
    -> ?tags:Logs.Tag.set
    -> ?read_buffer_size:int
    -> ?upgrade:(flow -> unit)
    -> flow
    -> unit Miou.t
end

val terminate : unit Miou.orphans -> unit
val clean : unit Miou.orphans -> unit
