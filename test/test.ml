open Onionctrl

let reply_of_string s =
  let open Astring in
  let lines = String.cuts ~sep:{|\r\n|} s in
  Reply.of_string_list lines |> Result.get_ok

let reply_of_string_result s =
  let open Astring in
  let lines = String.cuts ~sep:{|\r\n|} s in
  Reply.of_string_list lines

let test_reply = Alcotest.testable Reply.pp Reply.equal
let test_reply_parser = Alcotest.testable Reply.Parser.pp Reply.Parser.equal

(* Just check if we receive an Error *)
let test_reply_error =
  Alcotest.testable
    (fun ppf _ -> Format.fprintf ppf ": Expected an Error of some sort")
    (fun _ _ -> true)

let test_reply_single_midline () =
  let reply_s = {|250-OK|} in
  Alcotest.(check (result test_reply test_reply_error))
    "Single mid-line fails" (Error "foo")
    (reply_of_string_result reply_s)

let test_reply_end_line_order () =
  let reply_s = {|250_OK\r\n250-OK|} in
  Alcotest.(check (result test_reply test_reply_error))
    "End line not last fails" (Error "foo")
    (reply_of_string_result reply_s)

let test_reply_end_line_in_data () =
  let reply_s = {|250+foo=\r\nbar\r\n250 OK\r\n.\r\n250 OK|} in
  let reply_t =
    Reply.empty
    |> Reply.add_data [ "foo="; "bar"; "250 OK" ]
    |> Reply.add_end_line 250 "OK"
  in
  Alcotest.(check test_reply)
    "End line in data is just text" reply_t (reply_of_string reply_s)

let test_reply_parser_line_of_string () =
  let open Reply.Parser in
  let tests =
    [
      ("250 OK", Read_line, "250 OK", End_line (250, "OK"));
      ( "250+ Data_start",
        Read_line,
        "250+SOME_DATA=",
        Data_start (250, "SOME_DATA=") );
      ("250- is Mid_line", Read_line, "250-OK", Mid_line (250, "OK"));
      ("Empty string", Read_line, "", Parse_error);
      ("Data_end while Read_line is Parse_error", Read_line, ".", Parse_error);
      (". is Data_end", Read_data, ".", Data_end);
      ( "Mid_line data is just Data_line in Data",
        Read_data,
        "250-OK",
        Data_line "250-OK" );
      ("Empty string in Data is OK", Read_data, "", Data_line "");
    ]
  in
  List.iter
    (fun (test_name, current_state, input, expected_result) ->
      Alcotest.(check test_reply_parser)
        test_name
        (line_of_string current_state input)
        expected_result)
    tests

let test_test1 () =
  let reply_s =
    {|250-foobar\r\n250+stuff=\r\nding\r\ndong\r\n.\r\n250-baz\r\n250+garlic=\r\nbla\r\noasis\r\n.\r\n250 OK|}
  in
  let reply_t =
    Reply.empty
    |> Reply.add_mid_line 250 "foobar"
    |> Reply.add_mid_line 250 "baz"
    |> Reply.add_data [ "stuff="; "ding"; "dong" ]
    |> Reply.add_data [ "garlic="; "bla"; "oasis" ]
    |> Reply.add_end_line 250 "OK"
  in
  Alcotest.(check test_reply) "Test 1" reply_t (reply_of_string reply_s)

let test_test2 () =
  let reply_s =
    {|250-PROTOCOLINFO 1\r\n250-AUTH METHODS=COOKIE,SAFECOOKIE COOKIEFILE="/run/tor/control.authcookie"\r\n250-VERSION Tor="0.4.5.5-rc"\r\n250 OK|}
  in
  let reply_t =
    Reply.empty
    |> Reply.add_mid_line 250 "PROTOCOLINFO 1"
    |> Reply.add_mid_line 250
         "AUTH METHODS=COOKIE,SAFECOOKIE \
          COOKIEFILE=\"/run/tor/control.authcookie\""
    |> Reply.add_mid_line 250 "VERSION Tor=\"0.4.5.5-rc\""
    |> Reply.add_end_line 250 "OK"
  in
  Alcotest.(check test_reply) "Test 2" reply_t (reply_of_string reply_s)

let test_protocol_info = Alcotest.testable Protocol_info.pp Protocol_info.equal

let test_test3 () =
  let reply_s =
    {|250-PROTOCOLINFO 1\r\n250-AUTH METHODS=COOKIE,SAFECOOKIE COOKIEFILE="/run/tor/control.authcookie"\r\n250-VERSION Tor="0.4.5.5-rc"\r\n250 OK|}
  in
  let reply_t =
    {
      Protocol_info.pi_version = [ 1 ];
      auth_methods = [ Cookie; Safecookie ];
      cookiefile = "/run/tor/control.authcookie";
      tor_version = "0.4.5.5-rc";
    }
  in
  Alcotest.(check test_protocol_info)
    "Test 3" reply_t
    (reply_of_string reply_s |> Protocol_info.of_reply)

let () =
  let open Alcotest in
  run "onionctrl"
    [
      ( "Reply",
        [
          test_case "Single mid-line fails" `Quick test_reply_single_midline;
          test_case "End line not last fails" `Quick test_reply_end_line_order;
          test_case "End line in data is just text" `Quick
            test_reply_end_line_in_data;
          test_case "Reply Parser state machine" `Quick
            test_reply_parser_line_of_string;
          test_case "Test 1" `Quick test_test1;
          test_case "Test 2" `Quick test_test2;
        ] );
      ( "Protocol info",
        [
          test_case "Test 3" `Quick test_test3
          (* test_case "Quit parser" `Quick test_quit_parser; *)
          (* test_case "Protocol_info parser" `Quick test_protcol_info_parser; *);
        ] );
    ]
