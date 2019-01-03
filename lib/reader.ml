module AB = Angstrom.Buffered

type t =
  { parser : unit Angstrom.t
  ; mutable parse_state : unit AB.state
  ; mutable closed      : bool }

let create frame_handler =
  let parser =
    let open Angstrom in
    Websocket.Frame.parse
    >>| fun frame ->
      let is_fin = Websocket.Frame.is_fin frame in
      let opcode = Websocket.Frame.opcode frame in
      Websocket.Frame.unmask frame;
      Websocket.Frame.with_payload frame ~f:(frame_handler ~opcode ~is_fin)
  in
  { parser
  ; parse_state = AB.parse parser
  ; closed      = false
  }
;;

let rec read t input =
  match t.parse_state with
  | Fail _ -> ()
  | Done ({ buf; off; len }, ()) ->
    let parse_state' = AB.parse t.parser in
    t.parse_state <- parse_state';
    read_bigstring t buf ~off ~len;
    read t input
  | Partial continue ->
    let parse_state' = continue input in
    t.parse_state <- parse_state'

and read_bigstring t bigstring ~off ~len =
  let bigstring = Bigstring.sub bigstring ~off ~len in
  read t (`Bigstring bigstring)
