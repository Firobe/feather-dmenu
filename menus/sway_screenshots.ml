open Feather
open Feather_dmenu
open Dmenu
open Infix

let (let*) = Result.bind

let misc = ["-kb-move-char-back";"Control+b";"-kb-move-char-forward";
            "Control+f";"-kb-accept-entry"; "Right,Control+j,Control+m,Return,KP_Enter";
            "-kb-cancel";"Left,Escape,Control+g,Control+bracketleft" ]

let notify im txt =
  let icon =
    match im with
    | None -> "/usr/share/icons/Papirus/48x48/devices/camera-photo.svg"
    | Some filename -> filename
  in
  process "notify-send" [ "-t"; "4000"; "Screenshoter"; "-i"; icon; txt ] |> run

let get_date () = process "date" ["+%Y-%m-%d-%H-%M-%S"] |> collect stdout

let target_screen =
  (process "xdg-user-dir" ["PICTURES"] |> collect stdout) ^ "/screenshots"

let target_videos =
  (process "xdg-user-dir" ["VIDEOS"] |> collect stdout) ^ "/recordings"

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

let get_selection cmd =
  cmd |> collect everything
  |> get_result get_stdout get_stderr

let screens_geometry screens_geo =
  let w, h =
    List.fold_left (fun (aw,ah) screen_geo ->
        match String.split_on_char ' ' screen_geo with
        | [_;res] ->
          (match String.split_on_char 'x' res with
           | [w;h] -> aw+ int_of_string w, ah+int_of_string h
           | _ -> assert false)
        | _ -> assert false
      ) (0,0) screens_geo
  in
  Printf.sprintf "0,0 %dx%d" w h

let parse_geometry geo =
  if not @@ String.contains geo '\n' then
    geo
  else
    screens_geometry (String.split_on_char '\n' geo)

let cs cmd _ =
  let* geometry = get_selection cmd in
  let geometry = parse_geometry geometry in
  let* result =
    process "grim" [ "-g"; geometry; filename_screen ] &&. copy filename_screen
    |> collect status |> pack |> get_result
      (just @@ Printf.sprintf "Screenshot saved to %s and copied to clipboard!" target_screen)
      (just ("Action failed..."^geometry))
  in
  notify (Some filename_screen) result;
  Result.ok ()

let re sound cmd _ =
  let audio =
    match sound with
    | None -> []
    | Some source -> ["--audio="^source]
  in
  let* geometry = get_selection cmd in
  let* result =
    process "wf-recorder" (audio @ ["-g"; geometry; "-f"; filename_record])
    &&. copy filename_record |> collect status |> pack |> get_result
      (just @@ Printf.sprintf "Record saved to %s and copied to clipboard!" target_videos)
      (just "Action failed...")
  in
  notify None result;
  Result.ok ()

let clip () = process "slurp" ["-d";"-b00000044";"-c0092dcff";"-w 2"]

let rec_pid () =
  process "pidof" ["wf-recorder"]

let stop pid =
  process "kill" ["-SIGINT";pid] |> collect status |> pack 
  |> get_result (just ()) (just "Killing wf-recorder failed")

let rec span ?(b=false) ?(a=false) str f =
  let b = if b then span " " "2" else "" in
  let a = if a then span " " "2" else "" in
  b^"<span font='"^f^"'>"^str^"</span>"^a

let select_window () = windows() |. process "slurp" []
let select_output () = outputs() |. process "slurp" []

let title = "Screenshoter"
let theme = "~/.config/rofi/action.rasi"

let rec main_menu () =
  Dmenu.menu ~title ~theme ~misc Dmenu.[
      entry ~style:`None ((span "" "13")^"  Take a screenshot !") (fun _ -> screenshot_menu ());
      entry ~style:`Urgent ((span ~a:true "" "12")^"  Record a video !") (fun _ -> ask_audio_menu ());
    ]
and screenshot_menu () =
  Dmenu.menu ~title:"Take a screenshot" ~theme ~misc ~on_exit:(`Custom main_menu) Dmenu.[
      entry ((span ~a:true "" "11")^(span " " "2")^"  Capture fullscreen") (cs (outputs()));
      entry ((span ~a:true "" "13")^"  Capture region") (cs (clip ()));
      entry ((span ~b:true "" "13")^"  Capture focused") (cs (focused()));
      entry ((span ~b:true "缾" "13")^"  Capture select window") (cs (select_window ()));
      entry ((span ~b:true ~a:true "" "11")^"  Capture select output") (cs (select_output()));
    ]
and ask_audio_menu () =
  Dmenu.menu ~title:"Should capture sound ?" ~misc ~on_exit:(`Custom main_menu) Dmenu.[
      entry ~style:`None ((span "墳" "13")^"  Yes") pulse_menu;
      entry ~style:`Urgent ((span "婢" "13")^"  No") record_menu;
    ]
and pulse_menu _ =
  let ids =
    process "pactl" [ "list"; "short" ; "sources" ]
    |. cut ~d:'\t' 1 |> collect stdout |> lines
  in
  let descriptions =
    process "pactl" [ "list"; "sources" ]
    |. grep "Description" |. cut ~d:':' 2
    |> collect stdout |> lines |> List.map String.trim
  in
  let prompts =
    (Dmenu.entry ~style:`Active "Default source" (record_menu ~sound:"default"))
    ::
    (List.map2 (fun id desc ->
         Dmenu.entry desc (record_menu ~sound:id)) ids descriptions)
  in
  Dmenu.menu ~title:"Select an audio source" ~misc ~on_exit:(`Custom ask_audio_menu) prompts
and record_menu ?sound _ =
  Dmenu.menu ~title:"Record a video" ~theme ~misc ~on_exit:(`Custom ask_audio_menu) Dmenu.[
      entry ((span ~a:true "" "13")^"  Record region") (re sound (clip ()));
      entry ((span ~a:true "" "13")^"  Record focused") (re sound (focused()));
      entry ((span ~b:true "缾" "13")^"  Record select window") (re sound (select_window ()));
      entry ((span ~b:true ~a:true "" "11")^"  Record select output") (re sound (select_output()));
    ]

let main =
  let stdout, status = rec_pid () |> collect stdout_and_status in
  match status with
  | 0 -> stop stdout |> Dmenu.catch_errors
  | _ -> main_menu () |> Dmenu.catch_errors
