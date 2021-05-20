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

let span str f r =
  "<span font='"^f^"' rise='"^r^"'>"^str^"</span>"

let main =
  let title = "Powermenu" in
  let theme = "~/.config/rofi/action.rasi" in
  Dmenu.menu ~title ~theme Dmenu.[
    entry ((span "" "13" "1000")^"  Lock")    lock;
    entry ((span "望" "13" "1000")^"  Suspend") (systemctl ["suspend"]);
    entry ((span "﫼" "13" "1000")^"  Log Out") exit;
    entry ~style:`Urgent ((span "累" "14" "1000")^"  Reboot") (systemctl ["reboot"]);
    entry ~style:`Urgent ((span " " "0.4" "1000")^(span "襤" "14" "1000")^"  Shutdown") (systemctl ["-i";"poweroff"]);
    empty_row;
    entry ~style:`Active ((span "輦" "13" "1000")^"  BitWarden") bw;
  ] |> Dmenu.catch_errors