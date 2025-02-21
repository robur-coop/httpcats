exception Closed_by_peer
(* NOTE(dinosaure): it may happen that it is impossible to [write] to a peer.
   The standard error is [EPIPE] as well as a [SIGPIPE] signal that we ignore
   (at the application level). The user must transform this error by raising the
   [Closed_by_peer] exception. In this way, the "Runtime" is informed that the
   connection has been closed. *)

module type S = sig
  type t

  val read : t -> ?off:int -> ?len:int -> bytes -> int
  val write : t -> ?off:int -> ?len:int -> string -> unit
  val close : t -> unit
  val shutdown : t -> [ `read | `write | `read_write ] -> unit
end
