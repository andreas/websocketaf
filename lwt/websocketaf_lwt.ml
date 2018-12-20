open Lwt.Infix

let sha1 s =
  s
  |> Digestif.SHA1.digest_string
  |> Digestif.SHA1.to_raw_string
  |> B64.encode ~pad:true

module Buffer : sig
  type t

  val create : int -> t

  val get : t -> f:(Lwt_bytes.t -> off:int -> len:int -> int) -> int
  val put : t -> f:(Lwt_bytes.t -> off:int -> len:int -> int Lwt.t) -> int Lwt.t

  val to_string : t -> string
end = struct
  type t =
    { buffer      : Lwt_bytes.t
    ; mutable off : int
    ; mutable len : int }

  let to_string t =
    let s = Lwt_bytes.(extract t.buffer t.off t.len |> to_string) in
    let buf = Buffer.create 8 in
    String.iter (fun c ->
      Buffer.add_string buf (Char.code c |> string_of_int);
      Buffer.add_char buf ' '
    ) s;
    Buffer.contents buf

  let create size =
    let buffer = Lwt_bytes.create size in
    { buffer; off = 0; len = 0 }

  let compress t =
    if t.len = 0
    then begin
      t.off <- 0;
      t.len <- 0;
    end else if t.off > 0
    then begin
      Lwt_bytes.blit t.buffer t.off t.buffer 0 t.len;
      t.off <- 0;
    end

  let get t ~f =
    let n = f t.buffer ~off:t.off ~len:t.len in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0
    then t.off <- 0;
    n

  let put t ~f =
    compress t;
    f t.buffer ~off:(t.off + t.len) ~len:(Lwt_bytes.length t.buffer - t.len)
    >>= fun n ->
    t.len <- t.len + n;
    Lwt.return n
end


let read fd buffer =
  Lwt.catch
    (fun () ->
      Buffer.put buffer ~f:(fun bigstring ~off ~len ->
        Lwt_bytes.read fd bigstring off len))
    (function
    | Unix.Unix_error (Unix.EBADF, _, _) as exn ->
      Lwt.fail exn
    | exn ->
      Lwt.async (fun () ->
        Lwt_unix.close fd);
      Lwt.fail exn)

  >>= fun bytes_read ->
  if bytes_read = 0 then
    Lwt.return `Eof
  else
    Lwt.return (`Ok bytes_read)

let shutdown socket command =
  try Lwt_unix.shutdown socket command
  with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()


module Server = struct
  let create_connection_handler ?config:_ ~websocket_handler ~error_handler:_ =
    fun client_addr socket ->
      let module Server_connection = Websocketaf.Server_connection in
      let connection =
        Server_connection.create
          ~sha1
          ~websocket_handler:(websocket_handler client_addr)
      in


      let read_buffer = Buffer.create 0x1000 in
      let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

      let rec read_loop () =
        let rec read_loop_step () =
          match Server_connection.next_read_operation connection with
          | `Read ->
            print_string "read_loop_step READ\n"; flush stdout;
            read socket read_buffer >>= begin function
            | `Eof ->
              Format.printf "> [EOF] %s\n" (Buffer.to_string read_buffer); flush stdout;
              Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
                Server_connection.read_eof connection bigstring ~off ~len)
              |> ignore;
              read_loop_step ()
            | `Ok _ ->
              Format.printf "> %s\n" (Buffer.to_string read_buffer); flush stdout;
              Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
                Server_connection.read connection bigstring ~off ~len)
              |> ignore;
              read_loop_step ()
            end

          | `Yield ->
            print_string "read_loop_step YIELD\n"; flush stdout;
            Server_connection.yield_reader connection read_loop;
            Lwt.return_unit

          | `Close ->
            print_string "read_loop_step CLOSE\n"; flush stdout;
            Lwt.wakeup_later notify_read_loop_exited ();
            if not (Lwt_unix.state socket = Lwt_unix.Closed) then begin
              shutdown socket Unix.SHUTDOWN_RECEIVE
            end;
            Lwt.return_unit
        in

        Lwt.async (fun () ->
          Lwt.catch
            read_loop_step
            (fun exn ->
              (*Server_connection.report_exn connection exn;*)
              Printexc.print_backtrace stdout;
              ignore(raise exn);
              Lwt.return_unit))
    in


    let writev = Faraday_lwt_unix.writev_of_fd socket in
    let write_loop_exited, notify_write_loop_exited = Lwt.wait () in

    let rec write_loop () =
      print_string "write_loop BEGIN\n"; flush stdout;
      let rec write_loop_step () =
        match Server_connection.next_write_operation connection with
        | `Write io_vectors ->
          print_string "write_loop_step WRITE\n"; flush stdout;
          List.iter (fun iovec ->
            Format.printf "< %s\n" (Bigstringaf.sub iovec.Httpaf.IOVec.buffer ~off:iovec.off ~len:iovec.len |> Httpaf.Bigstring.to_string);
          ) io_vectors;
          writev io_vectors >>= fun result ->
          Server_connection.report_write_result connection result;
          write_loop_step ()

        | `Yield ->
          print_string "write_loop_step YIELD\n"; flush stdout;
          Server_connection.yield_writer connection write_loop;
          Lwt.return_unit

        | `Close _ ->
          print_string "write_loop_step CLOSE\n"; flush stdout;
          Lwt.wakeup_later notify_write_loop_exited ();
          if not (Lwt_unix.state socket = Lwt_unix.Closed) then begin
            shutdown socket Unix.SHUTDOWN_SEND
          end;
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          write_loop_step
          (fun exn ->
            (*Server_connection.report_exn connection exn;*)
            Printexc.print_backtrace stdout;
            ignore(raise exn);
            Lwt.return_unit))
    in


    read_loop ();
    write_loop ();
    Lwt.join [read_loop_exited; write_loop_exited] >>= fun () ->

    if Lwt_unix.state socket <> Lwt_unix.Closed then
      Lwt.catch
        (fun () -> Lwt_unix.close socket)
        (fun _exn -> Lwt.return_unit)
    else
      Lwt.return_unit
end





module Client = struct
  let connect socket ~nonce ~host ~port ~resource ~error_handler ~websocket_handler =
    let module Client_connection = Websocketaf.Client_connection in
    let connection =
      Client_connection.create ~nonce ~host ~port ~resource ~sha1 ~error_handler ~websocket_handler in

    let read_buffer = Buffer.create 0x1000 in
    let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

    let rec read_loop () =
      let rec read_loop_step () =
        match Client_connection.next_read_operation connection with
        | `Read ->
          print_string "read_loop_step READ\n";
          read socket read_buffer >>= begin function
          | `Ok _ ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Client_connection.read connection bigstring ~off ~len
            )
            |> ignore;
            read_loop_step ()
          | `Eof ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Client_connection.read_eof connection bigstring ~off ~len)
            |> ignore;
            read_loop_step ()
          end

        | `Yield ->
          print_string "read_loop_step YIELD\n";
          Client_connection.yield_reader connection read_loop;
          Lwt.return_unit

        | `Close ->
          print_string "read_loop_step CLOSE\n";
          Lwt.wakeup_later notify_read_loop_exited ();
          if not (Lwt_unix.state socket = Lwt_unix.Closed) then begin
            shutdown socket Unix.SHUTDOWN_RECEIVE
          end;
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          read_loop_step
          (fun exn ->
            (*Client_connection.report_exn connection exn;*)
            Printexc.print_backtrace stdout;
            ignore(raise exn);
            Lwt.return_unit))
    in


    let writev iovecs =
      List.iter (fun (iovec : Lwt_bytes.t Faraday.iovec) ->
        let s = Lwt_bytes.(extract iovec.buffer iovec.off iovec.len |> to_string) in
        Format.printf "> %s\n" s
      ) iovecs;
      Faraday_lwt_unix.writev_of_fd socket iovecs
    in
    let write_loop_exited, notify_write_loop_exited = Lwt.wait () in

    let rec write_loop () =
      let rec write_loop_step () =
        flush stdout;
        match Client_connection.next_write_operation connection with
        | `Write io_vectors ->
          print_string "write_loop_step WRITE\n";
          writev io_vectors >>= fun result ->
          Client_connection.report_write_result connection result;
          write_loop_step ()

        | `Yield ->
          print_string "write_loop_step YIELD\n";
          Client_connection.yield_writer connection write_loop;
          Lwt.return_unit

        | `Close _ ->
          print_string "write_loop_step CLOSE\n";
          Lwt.wakeup_later notify_write_loop_exited ();
          if not (Lwt_unix.state socket = Lwt_unix.Closed) then begin
            shutdown socket Unix.SHUTDOWN_SEND
          end;
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          write_loop_step
          (fun exn ->
            (*Client_connection.report_exn connection exn;*)
            ignore(raise exn);
            Lwt.return_unit))
    in


    read_loop ();
    write_loop ();

    Lwt.join [read_loop_exited; write_loop_exited] >>= fun () ->

    if Lwt_unix.state socket <> Lwt_unix.Closed then
      Lwt.catch
        (fun () -> Lwt_unix.close socket)
        (fun _exn -> Lwt.return_unit)
    else
      Lwt.return_unit;
end
