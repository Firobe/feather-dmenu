open Feather
open Feather_dmenu

let get_result ok err out =
  match out.status with
  | 0 -> Result.ok ok
  | _ -> Result.error (`Msg err)

let action kind _ =
  process "i3exit" [ kind ] |> collect (fun id -> id)
  |> get_result () "Action failed..."

let launch_heater_menu _ =
  process "cactus_client.exe" [] |> run;
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
