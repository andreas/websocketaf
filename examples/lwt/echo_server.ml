let connection_handler : Unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t =
  let module Body = Httpaf.Body in
  let module Headers = Httpaf.Headers in
  let module Reqd = Httpaf.Reqd in
  let module Response = Httpaf.Response in
  let module Status = Httpaf.Status in

  let websocket_handler wsd =
    let frame ~opcode ~is_fin:_ bs ~off ~len =
      match opcode with
      | `Continuation
      | `Text
      | `Binary ->
        Printf.printf "ECHOING: %s\n" (Bigstringaf.sub bs ~off ~len |> Httpaf.Bigstring.to_string); flush stdout;
        Websocketaf.Wsd.schedule wsd bs ~kind:`Text ~off ~len
      | `Connection_close ->
        Websocketaf.Wsd.close wsd
      | `Ping ->
        Websocketaf.Wsd.send_ping wsd
      | `Pong
      | `Other _ ->
        ()
    in
    let eof () =
      Printf.printf "websocket handler EOF\n"; flush stdout
    in
    { Websocketaf.Server_connection.frame
    ; eof
    }
  in

  let error_handler _client_address ?request:_ error start_response =
    let response_body = start_response Headers.empty in

    begin match error with
    | `Exn exn ->
      Printf.printf "Exn: %s\n" (Printexc.to_string exn);
      flush stdout

    | #Status.standard as error ->
      Printf.printf "Standard error: %s\n" (Status.default_reason_phrase error);
      flush stdout
    end;

    Body.close_writer response_body
  in

  let sha1 s =
    s
    |> Digestif.SHA1.digest_string
    |> Digestif.SHA1.to_raw_string
    |> B64.encode ~pad:true
  in

  let request_handler _fd reqd =
    Websocketaf.Server_connection.upgrade_connection
      ~sha1
      ~websocket_handler
      reqd
  in

  Httpaf_lwt.Server.create_connection_handler
    ?config:None
    ~request_handler
    ~error_handler



let () =
  let open Lwt.Infix in

  let port = ref 8080 in
  Arg.parse
    ["-p", Arg.Set_int port, " Listening port number (8080 by default)"]
    ignore
    "Echoes websocket messages. Runs forever.";

  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, !port)) in

  Lwt.async begin fun () ->
    Lwt_io.establish_server_with_client_socket
      listen_address connection_handler
    >>= fun _server ->
      Printf.printf "Listening on port %i and echoing websocket messages.\n" !port;
      flush stdout;
      Lwt.return_unit
  end;

  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
