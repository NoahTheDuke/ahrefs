module Cli = Nb_chatlib.Cli

let main () =
  (let options = (Cli.parse Sys.argv) in
    (Cli.OptionBag.pp options))

let () = (main ())
