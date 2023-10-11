module type RUNTIME = sig
  type t

  val next_read_operation : t -> [ `Read | `Yield | `Close ]
  val read : t -> Bigstringaf.t -> off:int -> len:int -> int
  val read_eof : t -> Bigstringaf.t -> off:int -> len:int -> int
  val yield_reader : t -> (unit -> unit) -> unit

  val next_write_operation :
    t -> [ `Write of Bigstringaf.t Faraday.iovec list | `Close of int | `Yield ]

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  val yield_writer : t -> (unit -> unit) -> unit
  val report_exn : t -> exn -> unit
end

type protect =
  { protect : 'a 'b. orphans:unit Miou.orphans -> ('a -> 'b) -> 'a -> 'b }
[@@unboxed]

exception Flow of string

module type S = sig
  type conn
  type flow

  val run :
       conn
    -> ?give:Miou.Ownership.t list
    -> ?disown:(flow -> unit)
    -> read_buffer_size:int
    -> flow
    -> protect * unit Miou.t * (unit -> unit)
end

module Make (Flow : Flow.S) (Runtime : RUNTIME) :
  S with type conn = Runtime.t and type flow = Flow.t

val terminate : unit Miou.orphans -> unit
