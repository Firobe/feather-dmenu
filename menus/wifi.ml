open Feather
open Feather_dmenu
open Dmenu

let (let*) = Result.bind

let misc = ["-kb-move-char-back";"Control+b";"-kb-move-char-forward";
            "Control+f";"-kb-accept-entry"; "Right,Control+j,Control+m,Return,KP_Enter";
            "-kb-cancel";"Left,Escape,Control+g,Control+bracketleft" ]

let get_result ?(f=fun _ -> true) ?(expected=0) f_ok f_err out =
  if out.status = expected && f out then
    Result.ok (f_ok out)
  else Result.error (`Msg (f_err out))
let get_stdout {stdout; _} = stdout
let just x _ = x

let not_empty_stdout out = not (String.equal "" out.stdout)

let title = "Wifi"
let theme = "~/.config/rofi/action.rasi"

let parse_wifi str =
  let lines = String.split_on_char '\n' str in
  if List.length lines < 2 then Result.error (`Msg "Parsing wifi")
  else
    Result.ok (List.map (fun l -> String.split_on_char ' ' l) (List.tl lines))

let get_status () =
  String.equal (process "nmcli" ["radio";"wifi"] |> collect stdout) "enabled"

let get_status_str () =
  if get_status () then
    "Wifi enabled"
  else
    "Wifi disabled"

let get_curr_wifi () =
  if get_status () then begin
    let stdout = process "nmcli" ["con";"show";"-a"] |> collect stdout in
    if String.equal "" stdout then Result.ok "Disconnected"
    else begin
      let* name = parse_wifi stdout in
      if List.hd name = [] then
        Result.error (`Msg "Something bad happened")
      else
        Result.ok ("Connected to "^List.hd  @@ List.hd name)
    end
  end
  else
    Result.ok "Wifi disabled"

let get_title () =
  if not (get_status ()) then
    Result.ok "Wifi disabled"
  else
    let* msg = get_curr_wifi () in
    Result.ok msg

let switch_wifi state =
  process "nmcli" ["radio";"wifi";state] |> collect status |> pack
  |> get_result (just ()) (just @@ "Switch wifi to "^state^" failed :/")

let display_con () =
  process "nmcli" ["dev";"wifi"] |. process "cat" []
  |> collect stdout_and_status |> pack_out
  |> get_result ~f:not_empty_stdout get_stdout (just "Could not display connection list")

let rec main_menu ?msg () =
  let* msg = match msg with
      None -> get_title ()
    | Some msg -> Result.error (`Msg msg)
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
    let wifi_list = String.split_on_char '\n' wifi_list in
    let splitted_items =
      List.filter_map (fun s ->
          let s =
            Str.global_replace (Str.regexp "[ ]+") " " s
            |> String.split_on_char ' ' in
          if List.length s < 2 then None
          else Some (List.nth s 2)
        ) (List.tl wifi_list)
    in
    Dmenu.menu ~title:"Select wifi" ~theme ~msg:st ~misc
      ~on_exit:(`Custom (fun () -> main_menu ()))
      (List.map (fun name ->
           Dmenu.entry name (fun _ -> connect_to name)) splitted_items)
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
        let stdout, status =
          process "nmcli" ["--ask";"dev";"wifi";"con";name]
          |> collect stdout_and_status in
        echo (stdout^":"^Int.to_string status) |> collect Feather.status
      | Some pwd -> echo pwd |. process "nmcli" ["--ask";"dev";"wifi";"con";name]
                    |> collect status
    end
  in
  match out with
  | 0 -> Result.ok ()
  | _ -> ask_password name

let () =
  main_menu () |> Dmenu.catch_errors
