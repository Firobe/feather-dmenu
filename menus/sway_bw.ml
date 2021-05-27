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

let get_result ?(f=fun _ -> true) ok err out =
  match out.status with
  | 0 -> if f ok then Result.return ok else Result.fail (`Msg err)
  | _ -> Result.fail (`Msg err)

let get_result_stdout ?(f=fun _ -> true) cmd err =
  let out = cmd |> collect Feather.stdout in
  get_result ~f out.stdout err out

let not_empty str = not (String.is_empty str)

let array_from_name name items =
  let out =
    echo items
    |. process "jq" ["-r"; ". | map(select((.name == \""^name^"\") and (.type == 1)))"]
    |> collect Feather.stdout
  in
  get_result out.stdout "Could not convert name to array" out

let load_items session =
  let cmd = process "bw" ["list"; "items"; "--session"; session] |> write_stderr_to devnull  in
  get_result_stdout ~f:not_empty cmd "Could not load items"

let copy str =
  process "echo" ["-n";str] |. process "wl-copy" [] |> collect (fun id -> id)
  |> get_result () "Could not copy"

let paste () =
  let cmd = process "wl-paste" [] in
  get_result_stdout cmd "Could not paste"

let sleep time =
  process "sleep" [time] |> collect (fun id -> id)
  |> get_result () "Could not sleep"

let clear old =
  let* _ = sleep (Int.to_string clear_time) in
  let* curr = paste () in
  if String.equal curr old then begin
    process "wl-copy" ["--clear"] |> collect (fun id -> id)
    |> get_result () "Could not clear"
  end
  else Result.return ()

let get_pass item =
  let cmd = echo item |. process "jq" ["-r";".[0].login.password"] in
  get_result_stdout cmd "Could not get password"

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
  let cmd = process "keyctl" ["pipe";key_id] in
  get_result_stdout cmd "Could not get session"

let sync key_id =
  let open Infix in
  let* session = get_session key_id in
  process "bw" ["sync";"--session";session]
  |> collect Feather.(stdout <+> stderr) |> get_result () "Failed to sync Bitwarden"

let set_timeout key_id =
  if auto_lock > 0 then begin
    process "keyctl" ["timeout";key_id;(Int.to_string auto_lock)]
    |> collect (fun id -> id) |> get_result () "Could not set session timeout"
  end
  else Result.return ()

let check_password pwd =
  let cmd = process "bw" ["unlock";pwd] |> write_stderr_to devnull in
  let* str = get_result_stdout ~f:not_empty cmd "Invalid master password" in
  let cmd =
    echo str |. process "grep" ["export"]
    |. process "sed" [ "-E"; "s/.*export BW_SESSION=\"(.*==)\"$/\\1/"]
    |. process "keyctl" ["padd";"user";"bw_session";"@u"]
  in
  get_result_stdout cmd "Could not unlock vault"

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
  let cmd =
    echo items |.
    process "jq" ["-r";".[] | select( has( \"login\" ) ) | \"\\(.name)\""]
  in
  let* items_names = get_result_stdout cmd "Could not extract names" in
  let lines = String.split items_names ~on:'\n' in
  let title = "Name" in
  Dmenu.menu ~title ~msg ~misc:actions
    (List.map lines ~f:(fun l ->
         Dmenu.entry l (fun out -> on_rofi_exit key_id items out)))

and main_menu ?(msg="Unlock your vault") () =
  let title = "Master password" in
  let theme = "~/.config/rofi/pwd.rasi" in
  let misc = ["-password"; "-lines"; "0"] in
  Dmenu.menu ~title ~theme ~msg ~misc
    ~on_unknown:(fun out -> show_items ~pwd:out.stdout ()) []

and on_rofi_exit key_id items out =
  match out.status with
  | 0 -> copy_password out.stdout items
  | 10 -> let* _ = sync key_id in show_items ~key_id ()
  | 11 -> lock_vault ()
  | _ -> Result.fail (`Msg ("Rofi exited with error : "^(Int.to_string out.status)))

and lock_vault () =
  let open Infix in
  let* _ =
    process "keyctl" ["purge";"user";"bw_session"]
    |> collect Feather.(stdout <+> stderr)
    |> get_result () "Could not lock the vault" in
  main_menu ~msg:"Vault locked !" ()

let main () =
  let open Infix in
  if auto_lock = 0 then begin
    let _ =
      process "keyctl" ["purge";"user";"bw_session"]
      |> collect Feather.(stdout <+> stderr) in
    main_menu ()
  end
  else
    let out =
      process "keyctl" ["request";"user";"bw_session"]
      |> collect Feather.(stdout <+> stderr)
    in
    if out.status <> 0 then
      main_menu ()
    else
      show_items ~key_id:out.stdout ()

let () =
  match main () with
  | Result.Ok () -> ()
  | Result.Error (`Msg err) -> Dmenu.error err
  | Result.Error _ -> Dmenu.error "Unknown error"