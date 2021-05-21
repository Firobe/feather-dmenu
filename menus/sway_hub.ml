open Feather
open Feather_dmenu

let get_result ok err =
  match last_exit () with
  | 0 -> Result.ok ok
  | _ -> Result.error (`Msg err)

let bw () =
  process "/home/thibault/Documents/clone/feather-dmenu/_build/default/sway_bw.exe" [] |> run;
  get_result () "Action failed..."

let screen () =
  process "/home/thibault/Documents/clone/feather-dmenu/_build/default/sway_screenshots.exe" [] |> run;
  get_result () "Action failed..."

let power () =
  process "/home/thibault/Documents/clone/feather-dmenu/_build/default/sway_powermenu.exe" [] |> run;
  get_result () "Action failed..."

let wifi () =
  process "/home/thibault/Documents/clone/feather-dmenu/_build/default/wifi.exe" [] |> run;
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
    entry (""^(span " " "3")^"  Screenshoter menu") screen;
    entry ((span " " "1")^(span "襤" "13")^"  Power menu") ~style:`Urgent power;
    entry ("直"^(span " " "3")^"  Wifi menu") ~style:`Active wifi;
    entry ((span " " "1")^"輦  BitWarden menu") ~style:`Active bw;
  ]

let main = main () |> Dmenu.catch_errors