module OptionBag = struct
  type t = {
    username : string option;
    port : int option;
    mode : [ `Client | `Server ];
  }

  let create () : t = { username = None; port = Some 5001; mode = `Server }

  let to_string { username; port; mode } =
    Printf.sprintf "{username: %s; port: %s; mode: [%s]}"
      (Option.value username ~default:"<no username>")
      (if Option.is_some port then string_of_int (Option.get port) else "<no port>")
      (match mode with
      | `Client -> "Client"
      | `Server -> "Server")

  let pp bag = print_endline @@ to_string bag
end

let specs (opts : OptionBag.t ref) =
  [
    ('u', "username", None, Some (fun s -> opts := { !opts with username = Some s }));
    ( 'p',
      "port",
      None,
      Some
        (fun s ->
          match int_of_string_opt s with
          | None -> ()
          | Some i -> opts := { !opts with port = Some i }) );
  ]

let parse_mode m =
  match String.lowercase_ascii m with
  | "server" -> `Server
  | "client" -> `Client
  | s ->
      Printf.printf "Incorrect mode given: %s. Defaulting to server" s;
      `Server

(** [parse input] parses an array of strings into a OptionBag*)
let parse input : OptionBag.t =
  let options = ref (OptionBag.create ()) in
  let modes : string list ref = ref [] in
  let () =
    Getopt.parse (specs options)
      (fun x -> modes := !modes @ [ x ])
      input 1
      (Array.length input - 1)
  in
  match !modes with
  | [] -> !options
  | [ m ] -> { !options with mode = parse_mode m }
  | m ->
      let mode_str = String.concat "; " m in
      let msg = Printf.sprintf "Too many modes: [%s]" mode_str in
      raise (Invalid_argument msg)
