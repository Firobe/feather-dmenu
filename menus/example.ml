open Feather_dmenu
open Feather
let process_arbitrary out =
  if String.equal "" out.stdout then failwith "LOL";
  Printf.printf "I got %s\n with exit status %d%!" out.stdout out.status;
  Result.ok ()

let trigger_exn _ =
  raise Not_found

let impossible _ =
  Feather.process "lolmdrwow" [] |> Feather.run;
  Result.ok ()

let main =
  Dmenu.menu ~on_unknown:process_arbitrary ~on_exit:`Error Dmenu.[
      default_entry "This entry uses the default action";
      empty_row;
      empty_row;
      entry ~style:`Urgent "This is a custom entry" (fun _ -> Result.ok ());
      entry ~style:`Active "This triggers an exception" trigger_exn;
      entry ~style:`Active "This calls a non-existing process" impossible;
    ] |> Dmenu.catch_errors
