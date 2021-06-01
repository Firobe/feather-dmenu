open Feather
open Feather_dmenu
open Infix

let (let*) = Result.bind
let clear_time = 5
let auto_lock = 900

type item = KeyId of string | Pwd of string

(* Keyboard shortcuts *)
let kb_sync = "Alt+s"
(* let kb_namesearch = "Alt+n" *)
let kb_lock = "Alt+l"
let kb_user = "Alt+u"
(* let kb_typeall = "Alt+1"
let kb_typeuser = "Alt+2"
let kb_typepass = "Alt+3" *)
let msg = " <b>Enter</b>: Copy pwd | <b>"^kb_sync^"</b>: sync vault
 <b>"^kb_user^"</b>: Copy id  | <b>"^kb_lock^"</b>: lock vault"

let actions =
  [ "-kb-custom-1";kb_sync;
    "-kb-custom-2";kb_lock;
    "-kb-custom-3";kb_user;]
    (* "-kb-custom-3";kb_typeall;
    "-kb-custom-4";kb_typeuser;
    "-kb-custom-5";kb_typepass] *)

let notify title txt =
  let time = clear_time * 1000 in
  process "notify-send" [ "-t"; Int.to_string time; title; txt ] |> run

let sudo args =
  process "sudo" args

let autotype t k = sudo ["ydotoll";t;k]

let get_result ?(f=fun _ -> true) ?(expected=0) f_ok f_err out =
  if out.status = expected && f out then
    Result.ok (f_ok out)
  else Result.error (`Msg (f_err out))
let get_stdout {stdout; _} = stdout
let just x _ = x

let not_empty_stdout out = not (String.equal "" out.stdout)

let array_from_name name items =
  echo items
  |. process "jq" ["-r"; ". | map(select((.name == \""^name^"\") and (.type == 1)))"]
  |> collect stdout
  |> get_result get_stdout (just "Could not convert name to array")

let copy str =
  process "echo" ["-n";str] |. process "wl-copy" [] |> collect only_status
  |> get_result (just ()) (just "Could not copy")

let paste () =
  process "wl-paste" [] |> collect stdout
  |> get_result get_stdout (just "Could not paste")

let sleep time =
  process "sleep" [time] |> collect only_status
  |> get_result (just ()) (just "Could not sleep")

let clear old =
  let* _ = sleep (Int.to_string clear_time) in
  let* curr = paste () in
  if String.equal curr old then begin
    process "wl-copy" ["--clear"] |> collect only_status
    |> get_result (just ()) (just "Could not clear")
  end
  else Result.ok ()

let get_session key_id =
  process "keyctl" ["pipe";key_id] |> collect stdout
  |> get_result get_stdout (just "Could not get session")

let load_items key_id =
  let* session = get_session key_id in
  process "bw" ["list"; "items"; "--session"; session] >! devnull
  |> collect stdout |>
  get_result ~f:not_empty_stdout get_stdout (just "Could not load items")

let get_pass key_id item =
  let* session = get_session key_id in
  process "bw" ["get";"password";item;"--session";session] |> collect stdout
  |> get_result get_stdout (just "Could not get password")

let get_user key_id item =
  let* session = get_session key_id in
  process "bw" ["get";"username";item;"--session";session] |> collect stdout
  |> get_result get_stdout (just "Could not get username")

let get_and_copy typ key_id name =
  let* copied =
    match typ with
    |`Pwd -> get_pass key_id name
    |`User -> get_user key_id name
  in
  let* _ = copy copied in
  let body =
    if Stdlib.(clear_time > 0) then
      ("Will be cleared in "^(Int.to_string clear_time)^"seconds.")
    else ""
  in
  notify (name^" copied !") body;
  if Stdlib.(clear_time > 0) then clear copied
  else Result.ok ()

let sync key_id =
  let* session = get_session key_id in
  process "bw" ["sync";"--session";session] >! devnull
  |> collect stdout
  |> get_result (just ()) (just "Failed to sync Bitwarden")

let set_timeout key_id =
  if Stdlib.(auto_lock > 0) then begin
    process "keyctl" ["timeout";key_id;(Int.to_string auto_lock)]
    |> collect only_status
    |> get_result (just ()) (just "Could not set session timeout")
  end
  else Result.ok ()

let check_password pwd =
  let* str =
    process "bw" ["unlock";pwd] >! devnull |> collect stdout
    |> get_result ~f:not_empty_stdout get_stdout (just "Invalid master password")
  in
  echo str |. process "grep" ["export"]
  |. process "sed" [ "-E"; "s/.*export BW_SESSION=\"(.*==)\"$/\\1/"]
  |. process "keyctl" ["padd";"user";"bw_session";"@u"]
  |> collect stdout
  |> get_result get_stdout (just "Could not unlock vault")

let rec show_items t =
  let* key_id =
    match t with
    | KeyId key -> Result.ok key
    | Pwd pwd -> check_password pwd
  in
  let* _ = set_timeout key_id in
  let* items = load_items key_id in
  let* items_names =
    echo items |. process "jq" ["-r";".[] | select( has( \"login\" ) ) | \"\\(.name)\""]
    |>collect stdout |> get_result get_stdout (just "Could not extract names")
  in
  let lines = String.split_on_char '\n' items_names in
  let title = "Name" in
  Dmenu.menu ~title ~msg ~misc:actions
    (List.map (fun l ->
         Dmenu.entry l (fun out -> on_rofi_exit key_id out)) lines)

and main_menu ?(msg="Unlock your vault") () =
  let title = "Master password" in
  let theme = "~/.config/rofi/pwd.rasi" in
  let misc = ["-password"; "-lines"; "0"] in
  Dmenu.menu ~title ~theme ~msg ~misc
    ~on_unknown:(fun out -> show_items (Pwd out.stdout)) []

and on_rofi_exit key_id out =
  match out.status with
  | 0 -> get_and_copy `Pwd key_id out.stdout
  | 10 -> let* _ = sync key_id in show_items (KeyId key_id)
  | 11 -> lock_vault ()
  | 12 -> get_and_copy `User key_id out.stdout
  | _ -> Result.error (`Msg ("Rofi exited with error : "^(Int.to_string out.status)))

and lock_vault () =
  let* _ =
    process "keyctl" ["purge";"user";"bw_session"] >! devnull
    |> collect stdout
    |> get_result (just ()) (just "Could not lock the vault")
  in
  main_menu ~msg:"Vault locked !" ()

let main () =
  if auto_lock = 0 then begin
    let _ =
      process "keyctl" ["purge";"user";"bw_session"] >! devnull
      |> collect stdout in
    main_menu ()
  end
  else
    let out =
      process "keyctl" ["request";"user";"bw_session"] >! devnull
      |> collect stdout
    in
    if out.status <> 0 then
      main_menu ()
    else
      show_items (KeyId out.stdout)

let main =
  main () |> Dmenu.catch_errors
