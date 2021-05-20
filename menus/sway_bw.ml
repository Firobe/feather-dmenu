open Feather
open Feather_dmenu
open Base

let (let*) v f = Result.bind v ~f
let clear_time = 5
let bw_hash = ref ""
let items = ref ""
let show_password = false
let auto_lock = 900

(* Keyboard shortcuts *)
let kb_sync = "Alt+r"
let kb_namesearch = "Alt+n"
let kb_lock = "Alt+l"
let kb_typeall = "Alt+1"
let kb_typeuser = "Alt+2"
let kb_typepass = "Alt+3"
let msg = "  <b>"^kb_sync^"</b>: sync  | <b>"^kb_typeall^"</b>: Type all
  <b>"^kb_lock^"</b>: lock  | <b>"^kb_typeuser^"</b>: Type user
  <b>"^kb_namesearch^"</b>: names | <b>"^kb_typepass^"</b>: Type pass"

let actions =
  [ "-kb-custom-1";kb_sync;
    "-kb-custom-2";kb_namesearch;
    "-kb-custom-3";kb_lock;
    "-kb-custom-4";kb_typeall;
    "-kb-custom-5";kb_typeuser;
    "-kb-custom-6";kb_typepass]

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

let ask_password reg_session =
  let title = "Master password" in
  let theme = "~/.config/rofi/pwd.rasi" in
  let misc = ["-password"; "-lines"; "0"] in
  let password _ str =
    let str =
      process "bw" ["unlock";str] |> write_stderr_to devnull |> collect_stdout
    in
    if String.equal str "" then
      Result.fail (`Msg "Invalid master password")
    else begin
      let session =
        echo str |. process "grep" ["export"]
        |. process "sed" [ "-E"; "s/.*export BW_SESSION=\"(.*==)\"$/\\1/"]
        |> collect_stdout
      in
      match last_exit () with
      | 0 when not (String.equal "" session) -> reg_session session
      | _ -> Result.fail (`Msg "Could not unlock vault")
    end
  in
  Dmenu.menu ~title ~theme ~misc ~on_none:`Error ~on_unknown:(`Custom password) []

let set_timeout key_id =
  if auto_lock > 0 then
    process "keyctl" ["timeout";key_id;(Int.to_string auto_lock)] |> run;
  bw_hash := process "keyctl" ["pipe";key_id] |> collect_stdout;
  Result.return ()

let get_session_key () =
  if auto_lock = 0 then begin
    process "keyctl" ["purge";"user";"bw_session"] |>
    stderr_to_stdout |> write_stdout_to devnull |> run;
    ask_password (fun session ->
        bw_hash := session;
        Result.return ())
  end
  else begin
    let key_id =
      process "keyctl" ["request";"user";"bw_session"]
      |> write_stderr_to devnull |> collect_stdout
    in
    if last_exit () <> 0 then begin
      ask_password (fun session ->
          echo session
          |. process "keyctl" ["padd";"user";"bw_session";"@u"]
          |> collect_stdout |> set_timeout)
    end
    else set_timeout key_id
  end

let array_from_name name =
  let str =
    echo !items
    |. process "jq" ["-r"; ". | map(select((.name == \""^name^"\") and (.type == 1)))"]
    |> collect_stdout
  in get_result str "Could not convert name to array"

(* let dedup_lines str =
   let items =
    echo str |. process "sort" [] |. process "uniq" ["-c"] |. process "sed" ["s/^\\s*1 //"]
    |. process "sed" ["-r";"s/^\\s*[0-9]+ /(+) /"] |> collect_stdout
   in
   let f str = not (String.equal "" str) in
   get_result ~f items "Could not dedup items" *)

let load_items () =
  let str =
    process "bw" ["list"; "items"; "--session"; !bw_hash]
    |> write_stderr_to devnull |> collect_stdout
  in
  let f str = not (String.equal "" str) in
  get_result ~f str "Could not load items"

let rofi_menu f prompt =
  let title = "Name" in
  let misc = ["-i"; "-no-custom"]@actions in
  Dmenu.menu ~title ~msg ~misc ~on_unknown:(`Custom f) (List.map prompt ~f:(fun l -> Dmenu.default_entry l))

let show_items f =
  let items =
    echo !items |.
    process "jq" ["-r";".[] | select( has( \"login\" ) ) | \"\\(.name)\""]
    |> collect_stdout
  in
  (* let* dedup_items = dedup_lines items in *)
  let lines = String.split items ~on:'\n' in
  rofi_menu f lines

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
  let* () = sleep (Int.to_string clear_time) in
  let* curr = paste () in
  if String.equal curr old then begin
    process "wl-copy" ["--clear"] |> run;
    get_result () "Could not clear"
  end
  else Result.return ()

let get_pass item =
  let pass = echo item |. process "jq" ["-r";".[0].login.password"] |> collect_stdout in
  get_result pass "Could not get password"

let copy_password name =
  let* item = array_from_name name in
  let* pass = get_pass item in
  let* () = copy pass in
  let body = if clear_time > 0 then ("Will be cleared in "^(Int.to_string clear_time)^"seconds.") else "" in
  notify (name^" copied !") body;
  if clear_time > 0 then clear pass
  else Result.return ()

let on_rofi_exit exit choice =
  match exit with
  | 0 -> copy_password choice
  | _ -> Result.fail (`Msg "Rofi exited with error")

let main () =
  let* () = get_session_key () in
  let* items' = load_items () in
  items := items';
  show_items on_rofi_exit

let () =
  match main () with
  | Result.Ok () -> ()
  | Result.Error (`Msg err) -> Dmenu.error err
  | Result.Error _ -> Dmenu.error "Unknown error"