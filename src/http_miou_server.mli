type error

val pp_error : error Fmt.t

type request = [ `V1 of Httpaf.Request.t | `V2 of H2.Request.t ]
type response = [ `V1 of Httpaf.Response.t | `V2 of H2.Response.t ]

type stream =
  { write_string : ?off:int -> ?len:int -> string -> unit
  ; write_bigstring : ?off:int -> ?len:int -> Bigstringaf.t -> unit
  ; close : unit -> unit
  }

val string : response -> string -> unit
val bigstring : response -> Bigstringaf.t -> unit
val stream : response -> stream

type error_handler =
  ?request:request -> error -> (H2.Headers.t -> stream) -> unit

type handler = request -> unit

val clear :
     ?stop:bool Atomic.t
  -> ?config:Httpaf.Config.t
  -> ?error_handler:error_handler
  -> handler:handler
  -> Miou_unix.file_descr
  -> unit
