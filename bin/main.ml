module Cli = Nb_chatlib.Cli
open Nb_chatlib.User
module Server = Nb_chatlib.Server

let main () =
  let options = Cli.parse Sys.argv in
  let user = User.create options in
  match options.mode with
  | `Client -> Server.create_client user
  | `Server -> Server.create_server user

let () = Lwt_main.run (main ())
