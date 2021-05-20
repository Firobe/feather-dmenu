open Feather_dmenu

let prompts = Dmenu.[
  default_entry "This entry uses the default action";
  empty_row;
  empty_row;
  entry ~style:`Urgent "This is a custom entry" (fun () -> Result.ok ());
]

let process_arbitrary exit str =
  if String.equal "" str then failwith "LOL";
  Printf.printf "I got %s\n with exit status %d%!" str exit;
  Result.ok ()

let main =
  Dmenu.menu ~on_unknown:process_arbitrary ~on_exit:`Error prompts
  |> Dmenu.catch_errors
