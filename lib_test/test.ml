let () =
  Alcotest.run "websocketaf" [
    "websocket", Websocket_test.suite;
  ]
