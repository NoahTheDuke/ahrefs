open Lwt.Syntax
open User
open Message

let ( >>= ) = Lwt.( >>= )

let print_new_connection (client_sockaddr : Unix.sockaddr) =
  Lwt_io.printf "Connected from: %s\n"
    (match client_sockaddr with
    | Unix.ADDR_INET (addr, port) ->
        Printf.sprintf "%s:%i" (Unix.string_of_inet_addr addr) port
    | Unix.ADDR_UNIX a -> a)

let get_message fd =
  let buf = Bytes.create 2048 in
  let* len = Lwt_unix.read fd buf 0 2048 in
  let msg =
    buf |> Bytes.to_string |> String.to_seq
    |> Seq.filter (fun s -> not (Char.equal s '\000'))
    |> String.of_seq |> String.trim |> Message.of_string
  in
  Lwt.return (len, msg)

let sockaddr_for_user (user : User.t) =
  Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, user.port)

let send_message (user : User.t) fd (msg : Message.t) =
  let msg_str =
    match msg with
    | Message.Ping -> Message.to_string msg
    | Message.Contents s -> Printf.sprintf "%s: %s\n" user.username s
  in
  let bmsg = Bytes.of_string msg_str in
  let msg_len = Bytes.length bmsg in
  let _ =
    match msg with
    | Message.Contents _ -> user.ping <- Unix.gettimeofday ()
    | _ -> ()
  in
  let* _ = Lwt_unix.write fd bmsg 0 msg_len in
  Lwt.return_unit

let close_socket (user : User.t) =
  let* _ = Lwt_unix.close user.socket in
  let* _ = Lwt_io.printl "Server closed. Exiting..." in
  exit 0

let rec handle_outgoing_message (user : User.t) fd =
  Lwt.catch
    (fun _ ->
      let* inp = Lwt_io.read_line_opt Lwt_io.stdin in
      match inp with
      | None -> Lwt.return_unit
      | Some s ->
          let msg = Message.of_string s in
          let* _ = send_message user fd msg in
          handle_outgoing_message user fd)
    (fun _ -> close_socket user)

let roundtrip_time (user : User.t) : float = Unix.gettimeofday () -. user.ping

let rec handle_incoming_message (user : User.t) fd =
  let* len, msg = get_message fd in
  match len with
  | 0 -> close_socket user
  | _ ->
      Lwt.catch
        (fun _ ->
          (match msg with
          | Message.Ping -> Lwt_io.printlf "Ping took %f sec" (roundtrip_time user)
          | Message.Contents msg ->
              let* _ = send_message user fd Message.Ping in
              Lwt_io.printl msg)
          >>= fun _ -> handle_incoming_message user fd)
        (fun _ -> close_socket user)

let create_server (user : User.t) =
  let sockaddr = sockaddr_for_user user in
  let _ = Lwt_unix.bind user.socket sockaddr in
  Lwt_unix.listen user.socket 5;
  let rec loop () =
    let* fd, sockaddr = Lwt_unix.accept user.socket in
    let* _ = print_new_connection sockaddr in
    Lwt.choose [ handle_outgoing_message user fd; handle_incoming_message user fd ]
    >>= fun _ -> loop ()
  in
  loop ()

let create_client (user : User.t) =
  let sockaddr = sockaddr_for_user user in
  let _ = Lwt_unix.connect user.socket sockaddr in
  let rec loop () =
    Lwt.choose
      [
        handle_outgoing_message user user.socket;
        handle_incoming_message user user.socket;
      ]
    >>= fun _ -> loop ()
  in
  loop ()
