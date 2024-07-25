module Cli = Getopt

module OptionBag = struct
  type t = {
    username : string option;
    port : string option;
    args : string list;
  }
  let create () : t =
    { username = None;
      port = None;
      args = []; }
  let pp ppf {username; port; args} = 
    Format.fprintf ppf "{username: %s; port: %s; args: [%s]}"
      (Option.value username ~default:"<no username>")
      (Option.value port ~default:"<12345>")
      (String.concat " " args)
end


let specs (opts: OptionBag.t ref) =
  [
    ('u', "username", None, Some (fun x -> opts := { !opts with username = Some x }));
    ('p', "port", None, Some (fun x -> opts := { !opts with port = Some x }));
  ]

let parse_argument (options : OptionBag.t ref) (x : string) =
  options := { !options with args = !options.args @ [x]}

let main () =
  let options = ref (OptionBag.create ()) in
  let () = Cli.parse_cmdline (specs options) (parse_argument options) in
  let options = !options in
  OptionBag.pp Format.std_formatter options

let () = main ()
