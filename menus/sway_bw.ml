open Feather
open Feather_dmenu
open Base

let (let*) v f = Result.bind v ~f
let clear_time = 5
let auto_lock = 900

(* Keyboard shortcuts *)
let kb_sync = "Alt+s"
(* let kb_namesearch = "Alt+n" *)
let kb_lock = "Alt+l"
let kb_typeall = "Alt+1"
let kb_typeuser = "Alt+2"
let kb_typepass = "Alt+3"
let msg = "  <b>"^kb_sync^"</b>: sync  | <b>"^kb_typeall^"</b>: Type all
  <b>"^kb_lock^"</b>: lock  | <b>"^kb_typeuser^"</b>: Type user
               | <b>"^kb_typepass^"</b>: Type pass"

let actions =
  [ "-kb-custom-1";kb_sync;
    "-kb-custom-2";kb_lock;
    "-kb-custom-3";kb_typeall;
    "-kb-custom-4";kb_typeuser;
    "-kb-custom-5";kb_typepass]

let notify title txt =
  let time = clear_time * 1000 in
  process "notify-send" [ "-t"; Int.to_string time; title; txt ] |> run

let sudo args =
  process "sudo" args

let autotype t k = sudo ["ydotoll";t;k]

let get_result ?(f=fun _ -> true) ok err =
  match last_exit () with
  | 0 -> if f ok then Result.return ok else Result.fail (`Msg err)
  | _ -> Result.fail (`Msg err)

let not_empty str = not (String.is_empty str)

let array_from_name name items =
  let str =
    echo items
    |. process "jq" ["-r"; ". | map(select((.name == \""^name^"\") and (.type == 1)))"]
    |> collect_stdout
  in
  get_result str "Could not convert name to array"

let load_items session =
  let str =
    process "bw" ["list"; "items"; "--session"; session]
    |> write_stderr_to devnull |> collect_stdout
  in
  get_result ~f:not_empty str "Could not load items"

let copy str =
  process "echo" ["-n";str] |. process "wl-copy" [] |> run;
  get_result () "Could not copy"

let paste () =
  let curr = process "wl-paste" [] |> collect_stdout in
  get_result curr "Could not paste"

let sleep time =
  process "sleep" [time] |> run;
  get_result () "Could not sleep"

let clear old =
  let* _ = sleep (Int.to_string clear_time) in
  let* curr = paste () in
  if String.equal curr old then begin
    process "wl-copy" ["--clear"] |> run;
    get_result () "Could not clear"
  end
  else Result.return ()

let get_pass item =
  let pass =
    echo item |. process "jq" ["-r";".[0].login.password"]
    |> collect_stdout
  in
  get_result pass "Could not get password"

let copy_password name items =
  let* item = array_from_name name items in
  let* pass = get_pass item in
  let* _ = copy pass in
  let body =
    if clear_time > 0 then
      ("Will be cleared in "^(Int.to_string clear_time)^"seconds.")
    else ""
  in
  notify (name^" copied !") body;
  if clear_time > 0 then clear pass
  else Result.return ()

let get_session key_id =
  let session = process "keyctl" ["pipe";key_id] |> collect_stdout in
  get_result session "Could not get session"

let sync key_id =
  let* session = get_session key_id in
  process "bw" ["sync";"--session";session]
  |> stderr_to_stdout |> write_stdout_to devnull |> run;
  get_result () "Failed to sync Bitwarden"

let set_timeout key_id =
  if auto_lock > 0 then begin
    process "keyctl" ["timeout";key_id;(Int.to_string auto_lock)] |> run;
    get_result () "Could not set session timeout"
  end
  else Result.return ()

let check_password pwd =
  let str =
    process "bw" ["unlock";pwd] |> write_stderr_to devnull
    |> collect_stdout
  in
  let* _ = get_result ~f:not_empty str "Invalid master password" in
  let key_id =
    echo str |. process "grep" ["export"]
    |. process "sed" [ "-E"; "s/.*export BW_SESSION=\"(.*==)\"$/\\1/"]
    |. process "keyctl" ["padd";"user";"bw_session";"@u"]
    |> collect_stdout
  in
  get_result key_id "Could not unlock vault"

let rec show_items ?key_id ?pwd () =
  let* key_id =
    match pwd,key_id with
    | None,Some key -> Result.return key
    | Some pwd, _ -> check_password pwd
    | _ -> assert false
  in
  let* _ = set_timeout key_id in
  let* session = get_session key_id in
  let* items = load_items session in
  let items_names =
    echo items |.
    process "jq" ["-r";".[] | select( has( \"login\" ) ) | \"\\(.name)\""]
    |> collect_stdout
  in
  let* _ = get_result () "Could not extract names" in
  let lines = String.split items_names ~on:'\n' in
  let title = "Name" in
  Dmenu.menu ~title ~msg ~misc:actions
    (List.map lines ~f:(fun l ->
         Dmenu.entry l (fun () -> on_rofi_exit key_id items (last_exit()) l)))

and main_menu ?(msg="Unlock your vault") () =
  let title = "Master password" in
  let theme = "~/.config/rofi/pwd.rasi" in
  let misc = ["-password"; "-lines"; "0"] in
  Dmenu.menu ~title ~theme ~msg ~misc
    ~on_unknown:(fun _ pwd -> show_items ~pwd ()) []

and on_rofi_exit key_id items code choice =
  match code with
  | 0 -> copy_password choice items
  | 10 -> let* _ = sync key_id in show_items ~key_id ()
  | 11 -> lock_vault ()
  | _ -> Result.fail (`Msg ("Rofi exited with error : "^(Int.to_string code)))

and lock_vault () =
  process "keyctl" ["purge";"user";"bw_session"]
  |> stderr_to_stdout |> write_stdout_to devnull |> run;
  let* _ = get_result () "Could not lock the vault" in
  main_menu ~msg:"Vault locked !" ()

let main () =
  if auto_lock = 0 then begin
    process "keyctl" ["purge";"user";"bw_session"] |>
    stderr_to_stdout |> write_stdout_to devnull |> run;
    main_menu ()
  end
  else
    let key_id =
      process "keyctl" ["request";"user";"bw_session"]
      |> write_stderr_to devnull |> collect_stdout
    in
    if last_exit () <> 0 then
      main_menu ()
    else
      show_items ~key_id ()

let () =
  match main () with
  | Result.Ok () -> ()
  | Result.Error (`Msg err) -> Dmenu.error err
  | Result.Error _ -> Dmenu.error "Unknown error"