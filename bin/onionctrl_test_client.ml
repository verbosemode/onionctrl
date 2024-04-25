(* The server responds with "250 OK" on success or "515 Bad authentication" if *)
(* the authentication cookie is incorrect.  Tor closes the connection on an *)
(* authentication failure. *)
(* TODO This will go into the Session module *)
let authenticate cookie socket =
  Onionctrl.(
    Socket.send_command (Command.make ~args:[ cookie ] "AUTHENTICATE") socket);
  match Onionctrl.Socket.recv_reply socket with
  | Ok { status = 250; _ } -> Ok ()
  (* Empty cookie *)
  | Ok { status = 515; _ } -> Error "Incorrect authentication cookie"
  (* 551 status_text: Invalid hexadecimal encoding.  Maybe you tried a plain text password?  If so, the standard requires that you put it in double quotes.*)
  | Ok { status; status_text; _ } ->
      Logs.debug (fun m ->
          m "Unhandeled AUTHENTICATE reply: status: %i status_text: %s" status
            status_text);
      Error "Unhandled AUTHENTICATE reply"
  | Error _ ->
      (* Log error: status, status_text *)
      Error "Protocol error: unable to parse reply"

let onion_controller () =
  let cookie = Onionctrl.load_cookie_file "/run/tor/control.authcookie" in
  let socket = Onionctrl.Socket.miou_unix_socket () in
  let sockaddr = Unix.ADDR_UNIX "/run/tor/control" in
  Miou_unix.connect socket sockaddr;
  (* Protocol info *)
  Onionctrl.Socket.send_command Onionctrl.Protocol_info.command socket;
  Onionctrl.Socket.recv_reply socket
  |> Result.get_ok |> Onionctrl.Protocol_info.of_reply
  |> Onionctrl.Protocol_info.show |> print_endline;
  (* Authenticate *)
  (match authenticate cookie socket with
  | Ok _ -> Logs.debug (fun m -> m "Authentication succsesfull")
  | Error _ -> Logs.debug (fun m -> m "Authentication failed"));
  (* Quit / Disconnect *)
  Onionctrl.Socket.send_command (Onionctrl.Command.make "QUIT") socket;
  Onionctrl.Socket.recv_reply socket
  |> Result.get_ok |> Onionctrl.Reply.show |> print_endline;
  Miou_unix.close socket

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  Miou_unix.run onion_controller
