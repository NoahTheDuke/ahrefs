open Lwt.Syntax
open User
open Message

let (>>=) = Lwt.(>>=)

let print_new_connection (client_sockaddr : Unix.sockaddr) =
  Lwt_io.printf "Connected from: %s\n"
    (match client_sockaddr with
    | Unix.ADDR_INET (addr, port) ->
        Printf.sprintf "%s:%i" (Unix.string_of_inet_addr addr) port
    | Unix.ADDR_UNIX a -> a)

let handle_connection conn =
  let client_socket, client_sockaddr = conn in
  let* () = print_new_connection client_sockaddr in
  let input_chan = Lwt_io.of_fd ~mode:Lwt_io.Input client_socket in
  let output_chan = Lwt_io.of_fd ~mode:Lwt_io.Output client_socket in
  Lwt.return (input_chan, output_chan)

let sockaddr_for_user (user : User.t) =
  Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, user.port)

let send_message (user : User.t) (msg : Message.t) =
  let msg_str = Printf.sprintf "%s: %s" user.username (Message.to_string msg) in
  let bmsg = Bytes.of_string msg_str in
  let msg_len = Bytes.length bmsg in
  Lwt.ignore_result (Lwt_unix.write user.socket bmsg 0 msg_len)

let rec handle_incoming_message (user : User.t) input_chan output_chan =
  Lwt_io.read_line_opt input_chan >>= fun inp ->
  match inp with
  | None ->
      print_endline "Server closed. Exiting...";
      Lwt.return_unit
  | Some s ->
      (match Message.of_string s with
      | Message.Contents msg ->
          send_message user Message.Ping;
          print_endline msg
      | p -> Printf.printf "%f: %s" (Unix.gettimeofday ()) (Message.to_string p));
      handle_incoming_message user input_chan output_chan

let create_server (user : User.t) =
  let sockaddr = sockaddr_for_user user in
  let _ = Lwt_unix.bind user.socket sockaddr in
  Lwt_unix.listen user.socket 5;
  let rec serve () =
    let* conn = Lwt_unix.accept user.socket in
    let* (input_chan, output_chan) = handle_connection conn in
    Lwt.choose [
      handle_incoming_message user input_chan output_chan;
    ]
    >>= fun _ -> serve()
  in
  serve ()
