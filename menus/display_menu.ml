open Feather
open Feather_dmenu

let deduplicate _ =
  process "bash" ["/home/virgile/.xprofile"] |> run;
  Result.ok ()

let duplicate _ =
  process "xrandr" ["--output"; "HDMI-0"; "--mode"; "1920x1080"; "--pos"; "0x0";
                    "--output"; "DVI-D-0"; "--mode"; "1920x1080"; "--pos"; "0x0"] |> run;
  Result.ok ()

let main =
  Dmenu.menu ~title:"Display menu" Dmenu.[
      entry " Reset screens" deduplicate;
      entry " Duplicate screens" duplicate;
    ] |> Dmenu.catch_errors
