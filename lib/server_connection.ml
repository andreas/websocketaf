type input_handlers =
  { frame : opcode:Websocket.Opcode.t -> is_fin:bool -> Bigstring.t -> off:int -> len:int -> unit
  ; eof   : unit                                                                          -> unit }

let passes_scrutiny _headers =
  true

let handle ~mode ~read_body ~write_body ~websocket_handler =
  let wsd = Wsd.create mode write_body in
  let { frame; eof = _ } = websocket_handler wsd in
  let reader = Reader.create frame in
  let rec schedule_read () =
    Httpaf.Body.schedule_read read_body
      ~on_eof:(fun () ->
        Printf.printf "on_eof\n"; flush stdout;
        (*
        Reader.read reader `Eof;
        eof ()
        *)
      )
      ~on_read:(fun bigstring ~off ~len ->
        Printf.printf "on_read\n"; flush stdout;
        Reader.read_bigstring reader bigstring ~off ~len;
        schedule_read ()
      )
  in
  schedule_read ()

let upgrade_connection ~sha1 ~websocket_handler reqd =
  let request = Httpaf.Reqd.request reqd in
  if passes_scrutiny request.headers then begin
    let key = Httpaf.Headers.get_exn request.headers "sec-websocket-key" in
    let accept = sha1 (key ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11") in
    let headers = Httpaf.Headers.of_list [
      "content-length", "3";
      "sec-websocket-accept", accept
    ] in
    let response = Httpaf.Response.create ~headers `OK in
    let write_body = Httpaf.Reqd.respond_with_streaming reqd response in
    let read_body = Httpaf.Reqd.request_body reqd in
    handle ~mode:`Server ~read_body ~write_body ~websocket_handler
  end
