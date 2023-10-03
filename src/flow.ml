module type S = sig
  type t
  type error

  val pp_error : error Fmt.t

  val read :
    ?read_buffer_size:int ->
    t ->
    ([ `Data of Cstruct.t | `End_of_input ], error) result

  val writev : t -> Cstruct.t list -> (unit, error) result
  val close : t -> unit
  val shutdown : t -> [ `Recv | `Send ] -> unit
end
