module TCP : Flow.S with type t = Miou_unix.file_descr
module TLS : module type of Tls_miou.Make (TCP)

val to_tls :
     Tls.Config.client
  -> ?host:[ `host ] Domain_name.t
  -> Miou_unix.file_descr
  -> (TLS.t, TLS.error) result

val epoch : TLS.t -> Tls.Core.epoch_data option
