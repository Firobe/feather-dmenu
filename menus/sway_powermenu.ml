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


let transparent = "00000000"
let transparent_black = "000000a0"
let trans_blue = "55cdfc"
let trans_pink = "f7a8b8"
let xeno_purple = "a317ff"
let xeno_light_purple = "9c84ff"
let xeno_orange = "ffb782"

let _ = debug := true

let lock _ =
  process "swaylock" [
    "--show-failed-attempts";
    "--screenshots";
    "--indicator-caps-lock";
    "--indicator-radius";"100";
    "--indicator-thickness"; "7";
    "--effect-pixelate"; "5";
    "--separator-color"; "00000000";
    "--effect-greyscale";
    "--fade-in"; "0.2";
    "--key-hl-color"; "a317ff";
    "--bs-hl-color"; "ffb782";
    "--line-color"; "00000000";
    "--text-color"; "9c84ff";
    "--ring-color"; "9c84ff";
    "--inside-color"; "000000a0";
    "--line-clear-color"; "00000000";
    "--text-clear-color"; "ffb782";
    "--ring-clear-color"; "ffb782";
    "--inside-clear-color"; "000000a0";
    "--line-caps-lock-color"; "00000000";
    "--text-caps-lock-color"; "ffb782";
    "--ring-caps-lock-color"; "ffb782";
    "--inside-caps-lock-color"; "000000a0";
    "--caps-lock-bs-hl-color"; "55cdfc";
    "--caps-lock-key-hl-color"; "ff6691";
    "--line-ver-color"; "00000000";
    "--text-ver-color"; "55cdfc";
    "--ring-ver-color"; "55cdfc";
    "--inside-ver-color"; "000000a0";
    "--line-wrong-color"; "00000000";
    "--text-wrong-color"; "f7a8b8";
    "--ring-wrong-color"; "f7a8b8";
    "--inside-wrong-color"; "000000a0"
  ] |> collect status |> pack
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
