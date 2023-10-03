type 'a t

val make : int -> 'a -> 'a t
val put : 'a t -> 'a -> unit
val get : 'a t -> 'a
