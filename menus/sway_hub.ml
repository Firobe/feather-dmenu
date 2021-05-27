open Feather
open Feather_dmenu

let get_result ok err out =
  match out.status with
  | 0 -> Result.ok ok
  | _ -> Result.error (`Msg err)

let bw _ =
  process "sway_bw.exe" [] |> collect (fun id -> id)
  |> get_result () "Action failed..."

let screen _ =
  process "sway_screenshots.exe" [] |> collect (fun id -> id)
  |> get_result () "Action failed..."

let power _ =
  process "sway_powermenu.exe" [] |> collect (fun id -> id)
  |> get_result () "Action failed..."

let wifi _ =
  process "wifi.exe" [] |> collect (fun id -> id)
  |> get_result () "Action failed..."

let app _ =
  process "rofi" ["-combi-modi";"drun#run";"-show";"combi";"-display-combi";"App launcher"]
  |> collect (fun id -> id) |> get_result () "Action failed..."

let span str f =
  "<span font='"^f^"'>"^str^"</span>"

let misc = ["-kb-move-char-back";"Control+b";"-kb-move-char-forward";
            "Control+f";"-kb-accept-entry"; "Right,Control+j,Control+m,Return,KP_Enter";
            "-kb-cancel";"Left,Escape,Control+g,Control+bracketleft" ]

let main () =
  let title = "Hub" in
  let theme = "~/.config/rofi/action.rasi" in
  Dmenu.menu ~title ~misc ~theme Dmenu.[
      entry ((span " " "1.8")^(span "" "13")^"  App launcher") app;
      entry (""^(span " " "2")^"  Screenshoter menu") screen;
      entry ((span " " "1.8")^(span "襤" "13")^"  Power menu") ~style:`Urgent power;
      entry ("直"^(span " " "4")^"  Wifi menu") ~style:`Active wifi;
      entry ((span "  " "0.8")^"輦  Password menu") ~style:`Active bw;
    ]

let main = main () |> Dmenu.catch_errors