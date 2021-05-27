open Feather
open Feather_dmenu
open Base

let (let*) v f = Result.bind v ~f

let misc = ["-kb-move-char-back";"Control+b";"-kb-move-char-forward";
            "Control+f";"-kb-accept-entry"; "Right,Control+j,Control+m,Return,KP_Enter";
            "-kb-cancel";"Left,Escape,Control+g,Control+bracketleft" ]

let get_result ?(f=fun _ -> true) ok err out =
  match out.status with
  | 0 -> if f ok then Result.return ok else Result.fail (`Msg err)
  | _ -> Result.fail (`Msg err)

let title = "Wifi"
let theme = "~/.config/rofi/action.rasi"

let parse_wifi str =
  let lines = String.split ~on:'\n' str in
  if List.length lines < 2 then Result.fail (`Msg "Parsing wifi")
  else
    Result.return (List.map (List.tl_exn lines) ~f:(fun l -> String.split ~on:' ' l))

let get_status () =
  let out = process "nmcli" ["radio";"wifi"] |> collect Feather.stdout in
  if String.equal out.stdout "enabled" then true else false

let get_status_str () =
  if get_status () then
    "Wifi enabled"
  else
    "Wifi disabled"

let get_curr_wifi () =
  if get_status () then begin
    let out = process "nmcli" ["con";"show";"-a"] |> collect Feather.stdout in
    if String.is_empty out.stdout then Result.return "Disconnected"
    else begin
      let* name = parse_wifi out.stdout in
      if List.is_empty (List.hd_exn name) then
        Result.fail (`Msg "Something bad happened")
      else
        Result.return ("Connected to "^List.hd_exn @@ List.hd_exn name)
    end
  end
  else
    Result.return "Wifi disabled"

let get_title () =
  if not (get_status ()) then
    Result.return "Wifi disabled"
  else
    let* msg = get_curr_wifi () in
    Result.return msg

let switch_wifi state =
  process "nmcli" ["radio";"wifi";state] |> collect (fun id -> id)
  |> get_result () ("Switch wifi to "^state^" failed :/")

let display_con () =
  let out =  process "nmcli" ["dev";"wifi"] |. process "cat" [] |> collect Feather.stdout in
  let f str = not (String.equal "" str) in
  get_result ~f out.stdout "Could not display connection list" out

let rec main_menu ?msg () =
  let* msg = match msg with
      None -> get_title ()
    | Some msg -> Result.return msg
  in
  Dmenu.menu ~title ~theme ~msg ~misc Dmenu.[
      entry ~style:`None "Change wifi state" (fun _ -> wifi_state_menu ());
      entry ~style:`Active "Select wifi" (fun _ -> wifi_select_menu ());
    ]

and wifi_state_menu () =
  let curr = get_status_str () in
  Dmenu.menu ~title:"Wifi state" ~theme ~msg:curr ~misc
    ~on_exit:(`Custom (fun () -> main_menu())) Dmenu.[
        entry ~style:`Active "直  Enable"
          (fun _ -> let* _ = switch_wifi "on" in wifi_state_menu());
        entry ~style:`Urgent "睊  Disable"
          (fun _ -> let* _ = switch_wifi "off" in wifi_state_menu());
      ]

and wifi_select_menu () =
  if not (get_status ()) then
    main_menu ~msg:"Enable wifi before selecting a connection" ()
  else begin
    let* st = get_title () in
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
    Dmenu.menu ~title:"Select wifi" ~theme ~msg:st ~misc
      ~on_exit:(`Custom (fun () -> main_menu ()))
      (List.map splitted_items ~f:(fun name ->
           Dmenu.entry name (fun _ -> connect_to name)
         ))
  end

and ask_password name =
  let title = "Wifi password" in
  let theme = "~/.config/rofi/pwd.rasi" in
  let misc = ["-password"; "-lines"; "0"] in
  Dmenu.menu ~title ~theme ~misc ~on_exit:(`Custom wifi_select_menu)
    ~on_unknown:(fun out -> connect_to ~pwd:out.stdout name) []

and connect_to ?pwd name =
  let out =
    begin match pwd with
      | None ->
        let out =
          process "nmcli" ["--ask";"dev";"wifi";"con";name]
          |> collect Feather.stdout in
        echo (out.stdout^":"^Int.to_string out.status) |> collect (fun id -> id)
      | Some pwd -> echo pwd |. process "nmcli" ["--ask";"dev";"wifi";"con";name]
                    |> collect (fun id -> id)
    end
  in
  match out.status with
  | 0 -> Result.return ()
  | _ -> ask_password name

let main =
  main_menu () |> Dmenu.catch_errors
