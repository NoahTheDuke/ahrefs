open Cli

let create_socket () = Lwt_unix.(socket PF_INET SOCK_STREAM) 0

module User = struct
  type t = {
    username : string;
    port : int;
    mode : [ `Client | `Server ];
    socket : Lwt_unix.file_descr;
  }

  let create (bag : OptionBag.t) : t =
    {
      username = Option.get bag.username;
      port = Option.get bag.port;
      mode = bag.mode;
      socket = create_socket ();
    }
end
