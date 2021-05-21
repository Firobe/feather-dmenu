open Feather
open Feather_dmenu
open Base

let (let*) v f = Result.bind v ~f

let misc = ["-kb-move-char-back";"Control+b";"-kb-move-char-forward";
            "Control+f";"-kb-accept-entry"; "Right,Control+j,Control+m,Return,KP_Enter";
            "-kb-cancel";"Left,Escape,Control+g,Control+bracketleft" ]

let get_result ?(f=fun _ -> true) ok err =
  match last_exit () with
  | 0 -> if f ok then Result.return ok else Result.fail (`Msg err)
  | _ -> Result.fail (`Msg err)

let title = "Wifi"
let theme = "~/.config/rofi/action.rasi"

let switch_wifi state () =
  process "nmcli" ["radio";"wifi";state] |> run;
  get_result () ("Switch wifi to "^state^" failed :/")

let display_con () =
  let ret =  process "nmcli" ["connection";"show"] |> collect_stdout in
  let f str = not (String.equal "" str) in
  get_result ~f ret "Could not display connection list"

let connect_to name ssid =
  process "nmcli" ["--ask";"connection";"up";ssid] |> run;
  get_result () ("Failed trying to connect to "^name)

let rec main_menu () =
  Dmenu.menu ~title ~theme ~misc Dmenu.[
      entry ~style:`Active "Change wifi state" wifi_state_menu;
      entry "Select wifi" wifi_select_menu;
    ]
and wifi_state_menu () =
  Dmenu.menu ~title:"Wifi state" ~theme ~misc ~on_exit:(`Custom main_menu) Dmenu.[
      entry ~style:`Active "Yes" (switch_wifi "on");
      entry ~style:`Urgent "No" (switch_wifi "off");
    ]
and wifi_select_menu () =
  let* wifi_list = display_con () in
  let wifi_list = String.split wifi_list ~on:'\n' in
  let splitted_items =
    List.filter_map (List.tl_exn wifi_list) ~f:(fun s ->
        let s = Str.global_replace (Str.regexp "[ ]+") " " s
          |> String.split ~on:' ' in
        if List.length s < 2 then None else
        Some (List.nth_exn s 0, List.nth_exn s 1)
      )
  in
  Dmenu.menu ~title:"Select wifi" ~theme ~misc ~on_exit:(`Custom main_menu)
    (List.map splitted_items ~f:(fun (name,ssid) ->
         Dmenu.entry name (fun () -> connect_to name ssid)
       ))

let main =
  main_menu () |> Dmenu.catch_errors
