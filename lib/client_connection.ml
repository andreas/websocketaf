type error =
  [ Httpaf.Client_connection.error
  | `Handshake_failure of Httpaf.Response.t * [`read] Httpaf.Body.t ]

type input_handlers =
  { frame : opcode:Websocket.Opcode.t -> is_fin:bool -> Bigstring.t -> off:int -> len:int -> unit
  ; eof   : unit                                                                          -> unit }

let passes_scrutiny ~accept headers =
  let upgrade              = Httpaf.Headers.get headers "upgrade"    in
  let connection           = Httpaf.Headers.get headers "connection" in
  let sec_websocket_accept = Httpaf.Headers.get headers "sec-websocket-accept" in
  sec_websocket_accept = Some accept
  && (match upgrade with
     | None         -> false
     | Some upgrade -> String.lowercase_ascii upgrade = "websocket")
  && (match connection with
     | None            -> false
     | Some connection -> String.lowercase_ascii connection = "upgrade")
;;

let random_string init =
  begin match init with
  | Some init -> Random.init init
  | None -> Random.self_init ()
  end;
  fun () ->
    Random.int32 Int32.max_int
;;

let handle ~mode ~read_body ~write_body ~websocket_handler =
  let wsd = Wsd.create mode write_body in
  let { frame; eof } = websocket_handler wsd in
  let reader = Reader.create frame in
  let rec schedule_read () =
    Httpaf.Body.schedule_read read_body
      ~on_eof:(fun () ->
        Reader.read reader `Eof;
        eof ()
      )
      ~on_read:(fun bigstring ~off ~len ->
        Reader.read_bigstring reader bigstring ~off ~len;
        schedule_read ()
      )
  in
  schedule_read ()
;;

let connect 
    ~nonce 
    ~host 
    ~port 
    ~resource
    ~sha1
    ~error_handler:(error_handler : error -> unit)
    ~websocket_handler
  =
  let request_body = ref None in
  let nonce = B64.encode nonce in
  let response_handler (response : Httpaf.Response.t) response_body =
    let accept = sha1 (nonce ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11") in
    match response.status with
    | `Switching_protocols when passes_scrutiny ~accept response.headers ->
      let mode = `Client (random_string None) in
      let write_body = match !request_body with Some b -> b | None -> assert false in
      handle ~mode ~read_body:response_body ~write_body ~websocket_handler
    | _                    ->
      error_handler (`Handshake_failure(response, response_body))
  in
  let headers =
    [ "upgrade"              , "websocket"
    ; "connection"           , "upgrade"
    ; "host"                 , String.concat ":" [ host; string_of_int port ]
    ; "sec-websocket-version", "13" 
    ; "sec-websocket-key"    , nonce
    ] |> Httpaf.Headers.of_list
  in
  let body, _connection =
    let error_handler = (error_handler :> Httpaf.Client_connection.error_handler) in
    Httpaf.Client_connection.request
      (Httpaf.Request.create ~headers `GET resource)
      ~error_handler
      ~response_handler
  in
  request_body := Some body
