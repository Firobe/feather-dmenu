open Feather_dmenu

let prompts = Dmenu.[
  default_entry "This entry uses the default action";
  empty_row;
  empty_row;
  entry ~style:`Urgent "This is a custom entry" (fun () -> Result.ok ());
]

let process_arbitrary exit str =
  Printf.printf "I got %s\n with exit status %d%!" str exit;
  Result.ok ()

let main =
  Dmenu.menu ~on_unknown:(`Custom process_arbitrary) prompts
  |> Dmenu.catch_errors
