open Feather_dmenu

let process_arbitrary exit str =
  if String.equal "" str then failwith "LOL";
  Printf.printf "I got %s\n with exit status %d%!" str exit;
  Result.ok ()

let trigger_exn () =
  raise Not_found

let impossible () =
  Feather.process "lolmdrwow" [] |> Feather.run;
  Result.ok ()

let main =
  Dmenu.menu ~on_unknown:process_arbitrary ~on_exit:`Error Dmenu.[
      default_entry "This entry uses the default action";
      empty_row;
      empty_row;
      entry ~style:`Urgent "This is a custom entry" (fun () -> Result.ok ());
      entry ~style:`Active "This triggers an exception" trigger_exn;
      entry ~style:`Active "This calls a non-existing process" impossible;
    ] |> Dmenu.catch_errors
