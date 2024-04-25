let src = Logs.Src.create "onionctrl"

module Log = (val Logs.src_log src : Logs.LOG)

let ( let* ) = Result.bind

let hexlify_string s =
  Hex.of_string s
  |> Hex.hexdump_s ~print_row_numbers:false ~print_chars:false
  |> Astring.String.filter Astring.Char.Ascii.is_hex_digit

let load_cookie_file filename =
  let cookie_len = 32 in
  let b = Bytes.create cookie_len in
  let ch = open_in filename in
  let b =
    Fun.protect
      ~finally:(fun () -> close_in ch)
      (fun () ->
        ignore (input ch b 0 cookie_len);
        b)
  in
  hexlify_string (Bytes.to_string b)

module Parser_utils = struct
  open Angstrom

  module Char = struct
    let is_digit = function '0' .. '9' -> true | _ -> false

    let is_alphanum = function
      | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> true
      | _ -> false

    let is_space = function ' ' -> true | _ -> false
    let is_cr = function '\r' -> true | _ -> false
  end

  let crlf = "\r\n"
  let quoted_string = char '"' *> take_while (fun c -> c != '"') <* char '"'

  let string_list sep is_valid_char =
    sep_by (char sep) (take_while is_valid_char)

  let status =
    let* d1 = satisfy Char.is_digit in
    let* d2 = satisfy Char.is_digit in
    let* d3 = satisfy Char.is_digit in
    return (Printf.sprintf "%c%c%c" d1 d2 d3 |> int_of_string)

  let take_any = take_while (fun _ -> true)
  let parse_string p s = parse_string ~consume:Consume.Prefix p s
end

module Reply = struct
  type t = {
    status : int;
    status_text : string;
    mid_lines : (int * string) list;
    data : string list list;
  }
  [@@deriving eq, show { with_path = false }]

  let empty = { status = 0; status_text = ""; mid_lines = []; data = [] }

  let create status status_text =
    { status; status_text; mid_lines = []; data = [] }

  let add_end_line status text msg = { msg with status; status_text = text }

  let add_mid_line status text msg =
    if List.length msg.mid_lines = 0 then
      { msg with mid_lines = [ (status, text) ] }
    else { msg with mid_lines = msg.mid_lines @ [ (status, text) ] }

  let add_data data msg =
    if List.length msg.data = 0 then { msg with data = [ data ] }
    else { msg with data = msg.data @ [ data ] }

  module Parser = struct
    module PU = Parser_utils

    type t =
      | End_line of int * string
      | Mid_line of int * string
      | Data_start of int * string
      | Data_line of string
      | Data_end
      | Parse_error
    [@@deriving eq, show { with_path = false }]

    type state = Read_line | Read_data | End_msg | Error_state
    [@@deriving show { with_path = false }]

    type actions =
      | Add_mid_line of int * string
      | Init_data of int * string
      | Add_data_line of string
      | Return_data
      | Return_reply of int * string
      | Raise_parse_error
    [@@deriving show { with_path = false }]

    let is_status_message_delimiter = function
      | ' ' | '-' | '+' -> true
      | _ -> false

    (* FIXME pass in parsers as parameter, based on current state? *)
    let line_of_string current_state s =
      let open Angstrom in
      let p =
        let* status = PU.status in
        let* delim = satisfy is_status_message_delimiter in
        let* text = PU.take_any in
        match delim with
        | ' ' -> return (End_line (status, text))
        | '-' -> return (Mid_line (status, text))
        | '+' -> return (Data_start (status, text))
        | _ -> assert false
      in
      let p_end_data =
        let* _ = char '.' in
        return Data_end
      in
      let p_any =
        let* text = PU.take_any in
        return (Data_line text)
      in
      let parsers =
        if current_state = Read_data then p_end_data <|> p_any else p
      in
      match parse_string ~consume:Consume.All parsers s with
      | Ok parsed_line -> parsed_line
      | Error _ -> Parse_error

    (* TODO not sure if repackaging data into actions is a good idea. probably not :D *)
    let transition state line =
      match (state, line) with
      | Read_line, Mid_line (status, text) ->
          (Read_line, Add_mid_line (status, text))
      | Read_line, Data_start (status, text) ->
          (Read_data, Init_data (status, text))
      | Read_line, End_line (status, text) ->
          (End_msg, Return_reply (status, text))
      | Read_data, Data_line text -> (Read_data, Add_data_line text)
      | Read_data, Data_end -> (Read_line, Return_data)
      | _ -> (Error_state, Raise_parse_error)

    let rec parse_lines current_state parsed_msg current_data lines =
      match (current_state, lines) with
      | End_msg, [] -> Ok parsed_msg
      | Error_state, _ | _, [] | End_msg, _ ->
          Error "Protocol_error: parsing failed"
      | _, h :: t -> (
          let line = line_of_string current_state h in
          let new_state, action = transition current_state line in
          match action with
          | Return_reply (status, text) ->
              parse_lines new_state (add_end_line status text parsed_msg) [] t
          | Add_mid_line (status, text) ->
              parse_lines new_state (add_mid_line status text parsed_msg) [] t
          | Init_data (_, text) -> parse_lines new_state parsed_msg [ text ] t
          | Add_data_line text ->
              parse_lines new_state parsed_msg (current_data @ [ text ]) t
          | Return_data ->
              parse_lines new_state (add_data current_data parsed_msg) [] t
          | Raise_parse_error -> parse_lines Error_state parsed_msg [] [])
  end

  let of_string_list lines = Parser.parse_lines Read_line empty [] lines
end

module Command = struct
  module PU = Parser_utils

  type t =
    | Single_line of string * string list
    | Multi_line of string * string list * string list

  let make ?(args = []) ?(data = []) keyword =
    if List.length data = 0 then Single_line (keyword, args)
    else Multi_line (keyword, args, data)

  let fold_args l keyword =
    let args = List.fold_left (fun a e -> a ^ " " ^ e) "" l in
    if String.length args > 0 then keyword ^ args ^ PU.crlf
    else keyword ^ PU.crlf

  let to_string = function
    | Single_line (keyword, args) -> fold_args args keyword
    | Multi_line (keyword, args, data) ->
        let firstline = "+" ^ fold_args args keyword in
        let lines = List.fold_left (fun a e -> a ^ e ^ PU.crlf) "" data in
        let lines = lines ^ "." ^ PU.crlf in
        firstline ^ lines
end

(* Mixed Socket / Wire format helper functions *)
module Socket = struct
  let miou_unix_socket () =
    let fd = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Miou_unix.of_file_descr ~non_blocking:true fd

  (* TODO do we need to ensure all bytes have been sent? send_all? *)
  let send_command command socket =
    let cmd_str = Command.to_string command in
    let cmd_str_len = String.length cmd_str in
    Miou_unix.write socket cmd_str 0 cmd_str_len

  (* Remove CRLF, whitespace stuff from data to simplify Angstrom parser *)
  let split_and_trim_message s =
    (* remove trailing 0 from socket buffer *)
    let null_char = Char.chr 0 in
    let s = Astring.String.trim ~drop:(fun c -> c = null_char) s in
    (* remove trailing empty lines *)
    let s = String.trim s in
    let l = String.split_on_char '\n' s in
    let l = List.map String.trim l in
    l

  (* TODO Do wee need to take care of not reading multiple messages at once? *)
  let recv socket =
    let buffer_len = 4095 in
    let buffer = Bytes.create buffer_len in
    let (_ : int) = Miou_unix.read socket buffer 0 buffer_len in
    Bytes.to_string buffer

  let recv_reply socket =
    Reply.of_string_list (split_and_trim_message (recv socket))

  (* combine / pair send_command with a corresponding recv_reply. Should be OK until async replies are implemented :-) *)
  (* let send_and_handle_reply = failwith "not implemented yet" *)
end

module Protocol_info = struct
  module Parser = struct
    open Angstrom
    module PU = Parser_utils

    let p_pi_version =
      let* version = string "PROTOCOLINFO " *> take_while PU.Char.is_digit in
      return (int_of_string version)

    let p_tor_version = string "VERSION " *> string "Tor=" *> PU.quoted_string

    let p_cookie_file =
      let* cookie_file = string "COOKIEFILE=" *> PU.quoted_string in
      return (Some cookie_file)

    let p_auth_methods =
      let* methods =
        string "AUTH " *> string "METHODS="
        *> PU.string_list ',' PU.Char.is_alphanum
      in
      let* cookie = char ' ' *> p_cookie_file <|> return None in
      return (methods, cookie)

    let parse_mid_line p a (_, line_string) =
      match a with
      | Ok _ as r -> r
      | Error _ -> Parser_utils.parse_string p line_string

    let parse_mid_lines p = List.fold_left (parse_mid_line p) (Error "")
    let parse_pi_version = parse_mid_lines p_pi_version
    let parse_tor_version = parse_mid_lines p_tor_version
    let parse_auth_methods = parse_mid_lines p_auth_methods
  end

  type auth_methods =
    | Null
    | Hashed_password
    | Cookie
    | Safecookie
    | Unknown of string
  [@@deriving eq, show { with_path = false }]

  let auth_method_of_string = function
    | "NULL" -> Null
    | "COOKIE" -> Cookie
    | "HASHEDPASSWORD" -> Hashed_password
    | "SAFECOOKIE" -> Safecookie
    | _ as s -> Unknown s

  type t = {
    pi_version : int list;
    tor_version : string;
    auth_methods : auth_methods list;
    cookiefile : string;
  }
  [@@deriving eq, show { with_path = false }]

  let command = Command.make ~args:[ "1" ] "PROTOCOLINFO"

  let of_reply (r : Reply.t) : t =
    let pi_version = Parser.parse_pi_version r.mid_lines in
    let tor_version = Parser.parse_tor_version r.mid_lines in
    let auth_methods = Parser.parse_auth_methods r.mid_lines in
    match (pi_version, tor_version, auth_methods) with
    | Ok pi_version, Ok tor_version, Ok (auth_methods, cookiefile) ->
        let auth_methods = List.map auth_method_of_string auth_methods in
        let cookiefile = Option.fold ~none:"" ~some:(fun s -> s) cookiefile in
        { pi_version = [ pi_version ]; tor_version; auth_methods; cookiefile }
    | _ -> failwith "TODO handle all cases."
end

(* module Session = struct
   ...
   * authenticate
   * automatically reconnect?
   ...
   end *)
