module IOVec = Httpaf.IOVec

type mode =
  [ `Client of unit -> int32
  | `Server
  ]

type t =
  { faraday : Faraday.t
  ; mode : mode
  ; response_body : [`write] Httpaf.Body.t
  }

let create mode response_body =
  { faraday = Faraday.create 14
  ; mode
  ; response_body
  }

let mask t =
  match t.mode with
  | `Client mask -> Some (mask ())
  | `Server -> None

let rec ready_to_write t =
  match Faraday.operation t.faraday with
  | `Yield ->
    ()
  | `Close ->
    Httpaf.Body.close_writer t.response_body
  | `Writev [] -> assert false    
  | `Writev (iovec::_) ->
    let { IOVec.buffer; off; len } = iovec in
    Httpaf.Body.schedule_bigstring t.response_body buffer ~off ~len;
    Faraday.shift t.faraday len;
    ready_to_write t

let schedule t ~kind:(kind : [`Text | `Binary]) payload ~off ~len =
  let mask = mask t in
  Websocket.Frame.schedule_serialize t.faraday ?mask ~is_fin:true ~opcode:(kind :> Websocket.Opcode.t) ~payload ~off ~len;
  ready_to_write t

let send_bytes t ~kind:(kind : [`Text | `Binary]) payload ~off ~len =
  let mask = mask t in
  Websocket.Frame.schedule_serialize_bytes t.faraday ?mask ~is_fin:true ~opcode:(kind :> Websocket.Opcode.t) ~payload ~off ~len;
  ready_to_write t

let send_ping t =
  Websocket.Frame.serialize_control t.faraday ~opcode:`Ping;
  ready_to_write t

let send_pong t =
  Websocket.Frame.serialize_control t.faraday ~opcode:`Pong;
  ready_to_write t

let flushed t f =
  Faraday.flush t.faraday f;
  ready_to_write t

let close t =
  Websocket.Frame.serialize_control t.faraday ~opcode:`Connection_close;
  Faraday.close t.faraday;
  ready_to_write t

let is_closed t =
  Faraday.is_closed t.faraday
