open Feather
open Infix
open Feather_dmenu
open Dmenu

let (let*) = Result.bind

(* Bind arrows to Enter and Escape *)
let misc = ["-kb-move-char-back";"Control+b";"-kb-move-char-forward";
            "Control+f";"-kb-accept-entry"; "Right,Control+j,Control+m,Return,KP_Enter";
            "-kb-cancel";"Left,Escape,Control+g,Control+bracketleft" ]

let notify txt =
  let success_icon = "/usr/share/icons/./Papirus/48x48/devices/camera-photo.svg"
  in process "notify-send"
    [ "-t"; "2000"; "Screenshoter"; "-i"; success_icon; txt ] |> run

let maim_format = "%wx%h+%x+%y"
let ffmpeg_format = "-video_size %wx%h -grab_x %x -grab_y %y"

let get_selection form =
    process "slop" [ "--highlight"; "--bordersize=3";
                     "--color=0.3,0.4,0.6,0.4"; "-f"; form ]
    |> collect everything
    |> get_result get_stdout get_stderr

(* Screenshot *)
let screenshot geom =
  process "maim" [ "-g"; geom; "--hidecursor" ]

let clipboard_img =
  process "xclip" [ "-selection"; "clipboard"; "-t"; "image/png"; "-i" ]

let take_screenshot _ =
  let* geometry = get_selection maim_format in
  let* result =
    screenshot geometry |. clipboard_img >! devnull
    |> collect status |> pack
    |> get_result (just "Image copied into clipboard") (just "Screenshot failed...")
  in notify result; Result.ok ()

(* Video *)
let default_path = "/tmp/out.mp4"
let delete_default = process "rm" ["-f"; default_path]
let screencast ~sound ~display geom =
  let audio_args = match sound with
    | None -> []
    | Some source -> ["-f"; "pulse"; "-i"; source]
  in
  delete_default &&.
  process "ffmpeg" (audio_args @ [
      "-f"; "x11grab"; "-framerate"; "60"] @ geom @ [
                      "-i"; display; "-loglevel"; "error"; "-c:v"; "libx264";
                      "-preset"; "ultrafast"; default_path ])

let rename_video out =
  if String.equal "" out.stdout then
    Result.error (`Msg "File name cannot be empty. Aborted.")
  else
    let home = Sys.getenv "HOME" in
    let full_path = home ^ "/" ^ out.stdout in
    mv default_path full_path |> run ;
    notify ("Video saved at " ^ full_path) ;
    Result.ok ()

let abort _ =
  delete_default |> run;
  notify "Video was aborted.";
  Result.ok ()

let take_video ?sound _ =
  let* geometry = get_selection ffmpeg_format in
  let args = String.split_on_char ' ' geometry in
  notify "Starting the video. Run again to stop.";
  let* display =
    Sys.getenv_opt "DISPLAY"
    |> Option.to_result ~none:(`Msg "Could not find DISPLAY") in
  let* _ =
    screencast ~sound ~display args |> collect status |> pack 
    |> get_result ~expected:255 (just ()) (just "Video failed...") in
  Dmenu.menu ~misc
    ~msg:"The video is in MP4 format. It will be placed into your home folder."
    ~on_unknown:rename_video ~on_exit:(`Custom abort)
    ~title:"File name" Dmenu.[entry ~style:`Urgent "Abort" abort]

let video_running () =
  let stdout, status = process "pidof" ["ffmpeg"] |> collect stdout_and_status in
  if status = 0 then Some stdout else None

let video_stop pid =
  process "kill" [ "-SIGINT"; pid] |> collect status |> pack
  |> get_result (just ()) (just "Error when trying to kill ffmpeg")

(* MENUS *)
let rec pulse_menu _ =
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
    (Dmenu.entry ~style:`Active "Default source" (take_video ~sound:"default"))
    ::
    (List.map2 (fun id desc ->
         Dmenu.entry desc (take_video ~sound:id)) ids descriptions)
  in
  Dmenu.menu ~misc ~on_exit:(`Custom video_menu) ~title:"Select an audio source" prompts

and video_menu () =
  Dmenu.menu ~on_exit:(`Custom menu) ~title:"Should capture sound" ~misc Dmenu.[
      entry "墳 Yes" pulse_menu;
      entry "婢 No" (take_video)
    ]

and menu () =
  Dmenu.menu ~title:"Select an action (applies to zone or window)" ~misc Dmenu.[
      entry "  Take a screenshot" take_screenshot;
      entry "  Take a video" (fun _ -> video_menu ()) 
    ]

let main =
  let default () =
    match video_running () with
    | Some pid -> video_stop pid
    | None -> menu ()
  in
  let result =
    if Array.length Sys.argv = 2 then
      match Sys.argv.(1) with
      | "screenshot" -> take_screenshot ()
      | _ -> default ()
    else default ()
  in Dmenu.catch_errors result
