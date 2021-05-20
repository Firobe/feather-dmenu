open Feather
open Feather_dmenu

let notify im txt =
  let icon =
    match im with
    | None -> "/usr/share/icons/Papirus/48x48/devices/camera-photo.svg"
    | Some filename -> filename
  in
  process "notify-send" [ "-t"; "4000"; "Screenshoter"; "-i"; icon; txt ] |> run

let get_result ok err =
  match last_exit () with
  | 0 -> Result.ok ok
  | _ -> Result.error (`Msg err)

let systemctl args () =
  process "systemctl" args |> run;
  get_result () "Action failed..."

let exit () =
  process "swaymsg" ["exit"] |> run;
  get_result () "Action failed..."

let lock () =
  process "swaylock-blur" [] |> run;
  get_result () "Action failed..."

let bw () =
  process "/home/thibault/Documents/clone/feather-dmenu/_build/default/sway_bw.exe" [] |> run;
  get_result () "Action failed..."

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
    empty_row;
    entry "輦  BitWarden" ~style:`Active bw;
  ] |> Dmenu.catch_errors