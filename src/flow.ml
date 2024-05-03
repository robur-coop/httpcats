module type S = sig
  type t

  val read : t -> ?off:int -> ?len:int -> bytes -> int
  val write : t -> ?off:int -> ?len:int -> string -> unit
  val close : t -> unit
  val shutdown : t -> [ `read | `write | `read_write ] -> unit
end
