open Feather
open Feather_dmenu

let get_result ok err =
  match last_exit () with
  | 0 -> Result.ok ok
  | _ -> Result.error (`Msg err)

let bw () =
  process "sway_bw.exe" [] |> run;
  get_result () "Action failed..."

let screen () =
  process "sway_screenshots.exe" [] |> run;
  get_result () "Action failed..."

let power () =
  process "sway_powermenu.exe" [] |> run;
  get_result () "Action failed..."

let wifi () =
  process "wifi.exe" [] |> run;
  get_result () "Action failed..."

let span str f =
  "<span rise='1000' font='"^f^"'>"^str^"</span>"

let misc = ["-kb-move-char-back";"Control+b";"-kb-move-char-forward";
            "Control+f";"-kb-accept-entry"; "Right,Control+j,Control+m,Return,KP_Enter";
            "-kb-cancel";"Left,Escape,Control+g,Control+bracketleft" ]

let main () =
  let title = "Hub" in
  let theme = "~/.config/rofi/action.rasi" in
  Dmenu.menu ~title ~misc ~theme Dmenu.[
    entry (""^(span " " "2")^"  Screenshoter menu") screen;
    entry ((span " " "1.5")^(span "襤" "13")^"  Power menu") ~style:`Urgent power;
    entry ("直"^(span " " "4")^"  Wifi menu") ~style:`Active wifi;
    entry ((span " " "2")^"輦  Password menu") ~style:`Active bw;
  ]

let main = main () |> Dmenu.catch_errors