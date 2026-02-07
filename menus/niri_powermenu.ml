open Feather
open Feather_dmenu
open Dmenu

let systemctl args _ =
  process "systemctl" args |> collect status |> pack
  |> get_result (just ()) (just "Action failed...")

let exit _ =
  process "quit" [] |> collect status |> pack
  |> get_result (just ()) (just "Action failed...")

let lock _ =
  process "qs" ["-c"; "ii"; "ipc"; "call"; "lock"; "activate"]
  |> collect status |> pack
  |> get_result (just ()) (just "Action failed...")

let span str f =
  "<span rise='1000' font='"^f^"'>"^str^"</span>"

let () =
  let title = "Powermenu" in
  let theme = "~/.config/rofi/action.rasi" in
  Dmenu.menu ~title ~theme Dmenu.[
      entry "  Lock" lock;
      entry "望  Suspend" (systemctl ["suspend"]);
      entry "﫼  Log Out" exit;
      entry ((span "累" "13")^"  Reboot") ~style:`Urgent (systemctl ["reboot"]);
      entry ((span "襤" "13")^"  Shutdown") ~style:`Urgent (systemctl ["-i";"poweroff"]);
    ] |> Dmenu.catch_errors
