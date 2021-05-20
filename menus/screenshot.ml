open Feather
open Infix
open Feather_dmenu

let (let*) = Result.bind

let notify txt =
  let success_icon = "/usr/share/icons/./Papirus/48x48/devices/camera-photo.svg"
  in process "notify-send"
    [ "-t"; "2000"; "Screenshoter"; "-i"; success_icon; txt ] |> run

let get_result ok err =
  match last_exit () with
  | 0 -> Result.ok ok
  | _ -> Result.error (`Msg err)

let maim_format = "%wx%h+%x+%y"
let ffmpeg_format = "-video_size %wx%h -grab_x %x -grab_y %y"

let get_selection form =
  let out = process "slop" [ "--highlight"; "--bordersize=3";
                             "--color=0.3,0.4,0.6,0.4"; "-f"; form ]
            |> stderr_to_stdout |> collect_stdout in
  get_result out out

(* Screenshot *)
let screenshot geom =
  process "maim" [ "-g"; geom; "--hidecursor" ]

let clipboard_img =
  process "xclip" [ "-selection"; "clipboard"; "-t"; "image/png"; "-i" ]

let take_screenshot () =
  let* geometry = get_selection maim_format in
  screenshot geometry |. clipboard_img >! devnull |> run ;
  let* result = get_result "Image copied into clipboard" "Screenshot failed..."
  in notify result; Result.ok ()

(* Video *)
let default_path = "/tmp/out.mp4"
let delete_default = process "rm" ["-f"; default_path]
let screencast ~sound geom =
  (* Problem: feather tries (and fortunately fails) to kill ffmpeg
   * at the end of the first run *)
  let audio_args = match sound with
    | None -> []
    | Some source -> ["-f"; "pulse"; "-i"; source]
  in
  delete_default &&.
  process "ffmpeg" (audio_args @ [
      "-f"; "x11grab"; "-framerate"; "60"] @ geom @ [
      "-i"; ":0.0"; "-loglevel"; "error"; "-c:v"; "libx264";
      "-preset"; "ultrafast"; default_path ])

let take_video ?sound () =
  let* geometry = get_selection ffmpeg_format in
  let args = String.split_on_char ' ' geometry in
  screencast ~sound args |> run_bg ;
  let* result = get_result
      "Video started. Call again to stop." "Video failed..." in
  notify result; Result.ok ()

let video_running () =
  let pid = process "pidof" ["ffmpeg"] |> collect_stdout in
  if last_exit () = 0 then Some pid else None

let rename_video _ new_name =
  let home = Sys.getenv "HOME" in
  let full_path = home ^ "/" ^ new_name in
  mv default_path full_path |> run ;
  notify ("Video saved at " ^ full_path) ;
  Result.ok ()

let abort _ _ =
  delete_default |> run;
  notify "Video was aborted.";
  Result.ok ()

let video_stop pid =
  process "kill" [ "-SIGINT"; pid] |> run;
  if last_exit () <> 0 then
    Result.error (`Msg "Error when trying to kill ffmpeg")
  else
    Dmenu.menu
      ~msg:"The video is in MP4 format. It will be placed into your home folder."
      ~on_unknown:(`Custom rename_video) ~on_none:(`Custom abort)
      ~title:"File name" Dmenu.[entry ~style:`Urgent "Abort" (abort 0)]

let pulse_menu () =
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
    (Dmenu.entry ~style:`Active "Default source" (take_video ~sound:"default"))
    ::
    (List.map2 (fun id desc ->
      Dmenu.entry desc (take_video ~sound:id)) ids descriptions)
  in
  Dmenu.menu ~title:"Select an audio source" prompts

let video_menu () =
  Dmenu.menu ~title:"Should capture sound" Dmenu.[
      entry "墳 Yes" pulse_menu;
      entry "婢 No" (take_video)
    ]

let menu () =
  Dmenu.menu ~title:"Select an action (applies to zone or window)" Dmenu.[
      entry "  Take a screenshot" take_screenshot;
      entry "  Take a video" video_menu
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