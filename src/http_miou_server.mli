type error

val pp_error : error Fmt.t

module Method = H2.Method
module Headers = H2.Headers
module Status = H2.Status

type request =
  { meth : Method.t; target : string; scheme : string; headers : Headers.t }

type stream =
  { write_string : ?off:int -> ?len:int -> string -> unit
  ; write_bigstring : ?off:int -> ?len:int -> Bigstringaf.t -> unit
  ; close : unit -> unit
  }

val string : status:Status.t -> ?headers:Headers.t -> string -> unit
val bigstring : status:Status.t -> ?headers:Headers.t -> Bigstringaf.t -> unit
val stream : ?headers:Headers.t -> Status.t -> stream

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
