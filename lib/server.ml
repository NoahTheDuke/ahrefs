open Lwt.Syntax
open User
open Message

let print_new_connection (client_sockaddr : Unix.sockaddr) =
  Lwt_io.printf "Connected from: %s\n"
    (match client_sockaddr with
    | Unix.ADDR_INET (addr, port) ->
        Printf.sprintf "%s:%i" (Unix.string_of_inet_addr addr) port
    | Unix.ADDR_UNIX a -> a)

let get_message client_socket =
  let buf = Bytes.create 2048 in
  let* len = Lwt_unix.read client_socket buf 0 2048 in
  let msg = Bytes.sub_string buf 0 len |> String.trim in
  let msg = Message.of_string msg in
  Lwt.return (len, msg)

let sockaddr_for_user (user : User.t) =
  Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, user.port)

let send_message (user : User.t) client_socket (msg : Message.t) =
  let msg_str =
    match msg with
    | Message.Contents s ->
        user.ping <- Unix.gettimeofday ();
        Printf.sprintf "%s: %s\n" user.username s
    | m -> Message.to_string m
  in
  let bmsg = Bytes.of_string msg_str in
  let msg_len = Bytes.length bmsg in
  let* _ = Lwt_unix.write client_socket bmsg 0 msg_len in
  Lwt.return_unit

let close_socket (user : User.t) client_socket =
  let* _ =
    match user.mode with
    | `Server -> Lwt.return_unit
    | `Client ->
        send_message user client_socket
          (Printf.sprintf "%s has disconnected." user.username |> Message.of_string)
  in
  let* _ = Lwt_unix.close user.socket in
  let* _ = Lwt_io.printl "Server closed. Exiting..." in
  exit 0

let rec handle_outgoing_message (user : User.t) client_socket =
  Lwt.catch
    (fun _ ->
      let* inp = Lwt_io.read_line_opt Lwt_io.stdin in
      match inp with
      | None -> Lwt.return_unit
      | Some s ->
          let msg = Message.of_string s in
          let* _ = send_message user client_socket msg in
          handle_outgoing_message user client_socket)
    (fun exn -> Lwt.fail exn)

let roundtrip_time (user : User.t) : float = Unix.gettimeofday () -. user.ping

let rec handle_incoming_message (user : User.t) client_socket =
  match Lwt_unix.state client_socket with
  | Lwt_unix.Opened -> (
      let* len, msg = get_message client_socket in
      match len with
      | 0 -> (
          match user.mode with
          | `Server -> Lwt.return_unit
          | `Client -> close_socket user client_socket)
      | _ ->
          Lwt.catch
            (fun _ ->
              let* _ =
                match msg with
                | Message.Ping ->
                    Lwt_io.printlf "Roundtrip took %f sec" (roundtrip_time user)
                | Message.Handshake s -> Lwt_io.printlf "%s has connected" s
                | Message.Contents msg ->
                    let* _ = send_message user client_socket Message.Ping in
                    Lwt_io.printl msg
              in
              handle_incoming_message user client_socket)
            (fun exn -> Lwt.fail exn))
  | _ -> close_socket user client_socket

let create_server (user : User.t) =
  let sockaddr = sockaddr_for_user user in
  let _ = Lwt_unix.bind user.socket sockaddr in
  Lwt_unix.listen user.socket 5;
  let rec loop () =
    let* client_socket, _client_sockaddr = Lwt_unix.accept user.socket in
    let* _ =
      Lwt.pick
        [
          handle_incoming_message user client_socket;
          handle_outgoing_message user client_socket;
        ]
    in
    loop ()
  in
  loop ()

let create_client (user : User.t) =
  let sockaddr = sockaddr_for_user user in
  let* _ = Lwt_unix.connect user.socket sockaddr in
  let* _ = send_message user user.socket (Message.Handshake user.username) in
  let rec loop () =
    let* _ =
      Lwt.pick
        [
          handle_incoming_message user user.socket;
          handle_outgoing_message user user.socket;
        ]
    in
    loop ()
  in
  loop ()
