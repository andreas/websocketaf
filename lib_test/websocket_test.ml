open Websocketaf

let suite = [
  "text", `Quick, begin fun () ->
    match Angstrom.parse_string Websocket.Frame.parse "\129\139\086\057\046\216\103\011\029\236\099\015\025\224\111\009\036" with
    | Ok frame ->
      Alcotest.(check bool) "has mask" true (Websocket.Frame.has_mask frame);
      Alcotest.(check int32) "mask" 1446588120l (Websocket.Frame.mask_exn frame);
      Websocket.Frame.unmask frame;
      let payload = Websocket.Frame.copy_payload_bytes frame |> Bytes.to_string in
      Alcotest.(check string) "payload" "1234567890\n" payload;
    | Error err -> failwith err
  end;
  "close", `Quick, begin fun () ->
    match Angstrom.parse_string Websocket.Frame.parse "\136\000" with
    | Ok frame ->
      Alcotest.(check int) "opcode" (Websocket.Opcode.to_int `Connection_close) Websocket.(Frame.opcode frame |> Opcode.to_int)
    | Error err -> failwith err
  end;
]
