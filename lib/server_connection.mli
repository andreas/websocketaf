type input_handlers =
  { frame : opcode:Websocket.Opcode.t -> is_fin:bool -> Bigstring.t -> off:int -> len:int -> unit
  ; eof   : unit                                                                          -> unit }

val upgrade_connection
  : sha1 : (string -> string)
  -> websocket_handler : (Wsd.t -> input_handlers)
  -> 'handle Httpaf.Reqd.t
  -> unit
