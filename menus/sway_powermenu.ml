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

let lock _ =
  process "swaylock" [
    "--daemonize";
    "--ignore-empty-password";
    "--show-failed-attempts";
    "--clock";
    "--grace"; "0";
    "--screenshots";
    "--indicator-caps-lock";
    "--effect-pixelate"; "6";
    (* "--effect-greyscale"; *)
    "--fade-in"; "0.2";
    "--bs-hl-color"; "DD532E";
    "--line-clear-color"; "000000";
    "--text-clear-color"; "f8f8f2";
    "--ring-clear-color"; "f8f8f2";
    "--inside-clear-color"; "000000a0";
    "--line-ver-color"; "000000";
    "--text-ver-color"; "2D6CA1";
    "--ring-ver-color"; "2D6CA1";
    "--inside-ver-color"; "000000a0";
    "--line-wrong-color"; "000000";
    "--text-wrong-color"; "DD532E";
    "--ring-wrong-color"; "DD532E";
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
