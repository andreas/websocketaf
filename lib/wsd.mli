type mode =
  [ `Client of unit -> int32
  | `Server
  ]

type t 

val create
  : mode
  -> [`write] Httpaf.Body.t
  -> t

val schedule
  :  t
  -> kind:[ `Text | `Binary ]
  -> Bigstring.t
  -> off:int
  -> len:int
  -> unit

val send_bytes
  :  t
  -> kind:[ `Text | `Binary ]
  -> Bytes.t
  -> off:int
  -> len:int
  -> unit

val send_ping : t -> unit
val send_pong : t -> unit

val flushed : t -> (unit -> unit) -> unit
val close   : t -> unit

val is_closed : t -> bool
