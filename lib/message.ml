module Message = struct
  type t = Ping | Contents of string

  let to_string = function
    | Ping -> "<acknowledged>"
    | Contents s -> s

  let to_bytes = function
    | Ping -> "<acknowledged>" |> Bytes.of_string
    | Contents s -> s |> Bytes.of_string

  let of_string = function
    | "<acknowledged>" -> Ping
    | s -> Contents s
end
