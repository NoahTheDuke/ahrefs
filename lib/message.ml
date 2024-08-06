module Message = struct
  type t =
    | Ping
    | Handshake of string
    | Contents of string

  let to_string = function
    | Ping -> "<acknowledged>"
    | Handshake s -> Printf.sprintf "<handshake> %s" s
    | Contents s -> s

  let to_bytes = function
    | Ping -> "<acknowledged>" |> Bytes.of_string
    | Handshake s -> Printf.sprintf "<handshake> %s" s |> Bytes.of_string
    | Contents s -> s |> Bytes.of_string

  let of_string = function
    | "<acknowledged>" -> Ping
    | str -> (
        match String.starts_with ~prefix:"<handshake> " str with
        | true -> Handshake (String.sub str 12 ((String.length str) - 12))
        | false -> Contents str)

  let msg_type = function
    | Ping -> "Ping"
    | Handshake _ -> "Handshake"
    | Contents _ -> "Contents"
end
