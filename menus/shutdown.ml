open Feather
open Feather_dmenu
open Dmenu

let action kind _ =
  process "i3exit" [ kind ] |> collect status
  |> pack |> get_result (just ()) (just "Action failed...")

let launch_heater_menu _ =
  process "cactus_client.exe" [] |> run;
  Result.ok ()

let launch_display_menu _ =
  process "display_menu.exe" [] |> run;
  Result.ok ()

let main =
  Dmenu.menu ~title:"Power menu" Dmenu.[
      entry ~style:`Urgent "襤 Shutdown" (action "shutdown");
      entry ~style:`Urgent "累 Reboot" (action "reboot");
      entry ~style:`Active " Log out" (action "logout");
      entry "望 Suspend" (action "suspend");
      entry " Lock" (action "lock");
      empty_row;
      entry " Heater management" launch_heater_menu;
      entry " Display management" launch_display_menu;
    ] |> Dmenu.catch_errors
