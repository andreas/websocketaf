type error =
  [ Httpaf.Client_connection.error
  | `Handshake_failure of Httpaf.Response.t * [`read] Httpaf.Body.t ]

type input_handlers =
  { frame : opcode:Websocket.Opcode.t -> is_fin:bool -> Bigstring.t -> off:int -> len:int -> unit
  ; eof   : unit                                                                          -> unit }

val connect 
  :  nonce             : string
  -> host              : string
  -> port              : int
  -> resource          : string
  -> sha1              : (string -> string)
  -> error_handler     : (error -> unit)
  -> websocket_handler : (Wsd.t -> input_handlers)
  -> unit
