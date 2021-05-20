open Feather
open Feather_dmenu

let get_result ok err =
  match last_exit () with
  | 0 -> Result.ok ok
  | _ -> Result.error (`Msg err)

let action kind () =
  process "i3exit" [ kind ] |> run;
  get_result () "Action failed..."

let launch_heater_menu () =
  process "cactus-client.ml" [] |> run;
  Result.ok ()

let main =
  Dmenu.menu ~title:"Power menu" Dmenu.[
      entry ~style:`Urgent "襤 Shutdown" (action "shutdown");
      entry ~style:`Urgent "累 Reboot" (action "reboot");
      entry ~style:`Active " Log out" (action "logout");
      entry "望 Suspend" (action "suspend");
      entry " Lock" (action "lock");
      empty_row;
      entry " Heater management" launch_heater_menu
    ] |> Dmenu.catch_errors
