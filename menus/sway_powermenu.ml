open Feather
open Feather_dmenu
open Dmenu

let notify im txt =
  let icon =
    match im with
    | None -> "/usr/share/icons/Papirus/48x48/devices/camera-photo.svg"
    | Some filename -> filename
  in
  process "notify-send" [ "-t"; "4000"; "Screenshoter"; "-i"; icon; txt ] |> run

let systemctl args _ =
  process "systemctl" args |> collect status |> pack
  |> get_result (just ()) (just "Action failed...")

let exit _ =
  process "swaymsg" ["exit"] |> collect status |> pack
  |> get_result (just ()) (just "Action failed...")

let lock _ =
  process "swaylock-blur" [] |> collect status |> pack
  |> get_result (just ()) (just "Action failed...")

let span str f =
  "<span rise='1000' font='"^f^"'>"^str^"</span>"

let main =
  let title = "Powermenu" in
  let theme = "~/.config/rofi/action.rasi" in
  Dmenu.menu ~title ~theme Dmenu.[
    entry "  Lock" lock;
    entry "望  Suspend" (systemctl ["suspend"]);
    entry "﫼  Log Out" exit;
    entry ((span "累" "13")^"  Reboot") ~style:`Urgent (systemctl ["reboot"]);
    entry ((span "襤" "13")^"  Shutdown") ~style:`Urgent (systemctl ["-i";"poweroff"]);
  ] |> Dmenu.catch_errors
