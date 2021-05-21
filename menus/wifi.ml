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
  let ret =  process "nmcli" ["dev";"wifi"] |. process "cat" [] |> collect_stdout in
  let f str = not (String.equal "" str) in
  echo ret |> run;
  get_result ~f ret "Could not display connection list"

let rec main_menu () =
  Dmenu.menu ~title ~theme ~misc Dmenu.[
      entry ~style:`None "Change wifi state" wifi_state_menu;
      entry ~style:`Active "Select wifi" wifi_select_menu;
    ]

and wifi_state_menu () =
  Dmenu.menu ~title:"Wifi state" ~theme ~misc ~on_exit:(`Custom main_menu) Dmenu.[
      entry ~style:`Active "直  Yes" (switch_wifi "on");
      entry ~style:`Urgent "睊  No" (switch_wifi "off");
    ]

and wifi_select_menu () =
  let* wifi_list = display_con () in
  let wifi_list = String.split wifi_list ~on:'\n' in
  let splitted_items =
    List.filter_map (List.tl_exn wifi_list) ~f:(fun s ->
        let s =
          Str.global_replace (Str.regexp "[ ]+") " " s
          |> String.split ~on:' ' in
        if List.length s < 2 then None
        else Some (List.nth_exn s 2)
      )
  in
  Dmenu.menu ~title:"Select wifi" ~theme ~misc ~on_exit:(`Custom main_menu)
    (List.map splitted_items ~f:(fun name ->
         Dmenu.entry name (fun () -> connect_to name)
       ))

and ask_password name =
  let title = "Wifi password" in
  let theme = "~/.config/rofi/pwd.rasi" in
  let misc = ["-password"; "-lines"; "0"] in
  Dmenu.menu ~title ~theme ~misc ~on_exit:(`Custom wifi_select_menu)
    ~on_unknown:(fun _ pwd -> connect_to ~pwd name) []

and connect_to ?pwd name =
  begin match pwd with
    | None ->
      let str = process "nmcli" ["--ask";"dev";"wifi";"con";name] |> collect_stdout in
      echo (str^":"^Int.to_string @@ last_exit()) |> run;
    | Some pwd -> echo pwd |. process "nmcli" ["--ask";"dev";"wifi";"con";name] |> run
  end;
  match last_exit () with
  | 0 -> Result.return ()
  | _ -> ask_password name

let main =
  main_menu () |> Dmenu.catch_errors
