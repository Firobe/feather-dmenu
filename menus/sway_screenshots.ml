open Feather
open Feather_dmenu
open Infix

let (let*) = Result.bind

let misc = ["-kb-move-char-back";"Control+b";"-kb-move-char-forward";
            "Control+f";"-kb-accept-entry"; "Right,Return,KP_Enter";
            "-kb-cancel";"Escape,Control+g,Control+bracketleft,Left" ]

let notify im txt =
  let icon =
    match im with
    | None -> "/usr/share/icons/Papirus/48x48/devices/camera-photo.svg"
    | Some filename -> filename
  in
  process "notify-send" [ "-t"; "4000"; "Screenshoter"; "-i"; icon; txt ] |> run

let get_date () = process "date" ["+%Y-%m-%d-%H-%M-%S"] |> collect_stdout

let target_screen =
  let out = process "xdg-user-dir" ["PICTURES"] |> collect_stdout in
  out^"/screenshots"

let target_videos =
  let out = process "xdg-user-dir" ["VIDEOS"] |> collect_stdout in
  out^"/recordings"

let focused () =
  process "swaymsg" ["-tget_tree"] |.
  process "jq" ["-r";".. | ((.nodes? + .floating_nodes?) // empty) | .[] | select(.focused and .pid) | .rect | \"\\(.x),\\(.y) \\(.width)x\\(.height)\""]

let outputs () =
  process "swaymsg" ["-tget_outputs"] |.
  process "jq" ["-r";".[] | select(.active) | .rect | \"\\(.x),\\(.y) \\(.width)x\\(.height)\""]

let windows () =
  process "swaymsg" ["-tget_tree"] |.
  process "jq" ["-r";".. | select(.pid? and .visible?) | .rect | \"\\(.x),\\(.y) \\(.width)x\\(.height)\""]

let () =
  mkdir_p target_screen |> run;
  mkdir_p target_videos |> run

let filename_screen = target_screen ^ "/scrn-" ^ (get_date()) ^ ".png"
let filename_record = target_videos ^ "/vid-" ^ (get_date()) ^ ".mp4"

let copy file = process "wl-copy" [] < file

let () = debug := true

let get_result ok err =
  match last_exit () with
  | 0 -> Result.ok ok
  | _ -> Result.error (`Msg err)

let get_selection cmd =
  let out = cmd |> stderr_to_stdout |> collect_stdout in
  get_result out out

let cs cmd () =
  let* geometry = get_selection cmd in
  process "grim" [ "-g"; geometry; filename_screen ] &&. copy filename_screen |> run;
  let* result = get_result
      (Printf.sprintf "Screenshot saved to %s and copied to clipboard!" target_screen)
      ("Action failed...") in
  notify (Some filename_screen) result;
  Result.ok ()

let re sound cmd () =
  let audio =
    match sound with
    | None -> []
    | Some source -> ["--audio="^source]
  in
  let* geometry = get_selection cmd in
  process "wf-recorder" (audio @ ["-g"; geometry; "-f"; filename_record]) &&. copy filename_record |> run;
  let* result = get_result (Printf.sprintf "Record saved to %s and copied to clipboard!" target_videos)
      ("Action failed...") in
  notify None result;
  Result.ok ()

let clip () = process "slurp" ["-d";"-b00000044";"-c0092dcff";"-w 2"]

let rec_pid () =
  process "pidof" ["wf-recorder"] |> collect_stdout

let stop pid =
  process "kill" ["-SIGINT";pid] |> run;
  match last_exit () with
  | 0 -> ()
  | _ -> Dmenu.error "Error when trying to kill wf-recorder"

let span str f r =
  "<span font='"^f^"' rise='"^r^"'>"^str^"</span>"

let select_window () = windows() |. process "slurp" []
let select_output () = outputs() |. process "slurp" []

let title = "Screenshoter"
let theme = "~/.config/rofi/action.rasi"

let rec main_menu () =
  Dmenu.menu ~title ~theme  Dmenu.[
      entry ~style:`Active ((span "" "13" "1000")^"  Take a screenshot !") screenshot_menu;
      entry ~style:`Urgent ((span "" "12" "1000")^(span " " "13" "0")^" Record a video !") ask_audio_menu;
    ]
and screenshot_menu () =
  Dmenu.menu ~title:"Take a screenshot" ~theme ~misc ~on_exit:(`Custom main_menu) Dmenu.[
      entry ((span "" "11" "1000")^(span " " "13" "0")^" Capture fullscreen") (cs (outputs()));
      entry ((span "" "13" "1000")^"  Capture region") (cs (clip ()));
      entry ((span "" "13" "1000")^"  Capture focused") (cs (focused()));
      entry ((span "缾" "13" "1000")^"  Capture select window") (cs (select_window ()));
      entry ((span "" "11" "1000")^(span " " "13" "0")^" Capture select output") (cs (select_output()));
    ]
and ask_audio_menu () =
  Dmenu.menu ~title:"Should capture sound ?" ~misc ~on_exit:(`Custom main_menu) Dmenu.[
      entry ~style:`Active "墳 Yes" pulse_menu;
      entry ~style:`Urgent "婢 No" record_menu;
    ]
and pulse_menu () =
  let ids =
    process "pactl" [ "list"; "short" ; "sources" ]
    |. cut ~d:'\t' 1 |> collect_lines
  in
  let descriptions =
    process "pactl" [ "list"; "sources" ]
    |. grep "Description" |. cut ~d:':' 2
    |> collect_lines |> List.map String.trim
  in
  let prompts =
    (Dmenu.entry ~style:`Active "Default source" (record_menu ~sound:"default"))
    ::
    (List.map2 (fun id desc ->
         Dmenu.entry desc (record_menu ~sound:id)) ids descriptions)
  in
  Dmenu.menu ~title:"Select an audio source" ~misc ~on_exit:(`Custom ask_audio_menu) prompts
and record_menu ?sound () =
  Dmenu.menu ~title:"Record a video" ~theme ~misc ~on_exit:(`Custom ask_audio_menu) Dmenu.[
      entry ((span "" "13" "1000")^"  Record region") (re sound (clip ()));
      entry ((span "" "13" "1000")^"  Record focused") (re sound (focused()));
      entry ((span "缾" "13" "1000")^"  Record select window") (re sound (select_window ()));
      entry ((span "" "11" "1000")^(span " " "13" "0")^" Record select output") (re sound (select_output()));
    ]

let main =debug := true;
  let rec_pid = rec_pid () in
  match last_exit () with
  | 0 -> stop rec_pid
  | _ -> main_menu () |> Dmenu.catch_errors
