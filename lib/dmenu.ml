open Feather
open Base

type 'e prompt = {
  label: string;
  style: [`Urgent | `Active | `None];
  f: (everything -> (unit, 'e) Result.t) option;
}

let get_choice input prompts =
  List.find prompts ~f:(fun p -> String.equal p.label input)

let gather_style select prompts =
  List.filter_mapi prompts ~f:(fun i e ->
      if Poly.(e.style = select) then Some (Int.to_string i) else None
    )
  |> String.concat ~sep:","

let wrap_user_fn f =
  try f ()
  with e ->
    let msg = "exception in user function -> " ^ (Exn.to_string_mach e) in
    Result.fail (`Msg msg)

let do_exit err_str = function
  | `Nothing -> Result.return ()
  | `Error -> Result.fail (`Msg err_str)
  | `Custom f -> wrap_user_fn f

let menu' title msg theme case on_exit on_unknown misc prompts =
  (* Concat choice\nchoice\nchoice... *)
  let input = prompts |> List.map ~f:(fun e -> e.label) |> String.concat ~sep:"\n" in
  (* Concat 1,3,4,... for urgent prompts *)
  let urgents = gather_style `Urgent prompts in
  let active = gather_style `Active prompts in
  let theme_l = match theme with None -> [] | Some t -> [ "-theme"; t ] in
  let msg_l = match msg with None -> [] | Some m -> [ "-mesg"; m ] in
  let allow_custom = match on_unknown with None -> "-no-custom" | Some _ -> "" in
  let case = if case then "" else "-i" in
  let misc = match misc with None -> [] | Some m -> m in
  let out =
    process "echo" [ "-e"; input ] |.
    process "rofi"
      (case :: allow_custom :: theme_l @ msg_l @ misc @
       [ "-markup-rows"; "-dmenu"; "-u"; urgents ; "-a"; active; "-p"; title ])
    |> collect everything in
  if String.equal out.stdout "" && out.status = 1 then
    do_exit "No action selected" on_exit
  else
    match Option.(get_choice out.stdout prompts >>= (fun a -> a.f)) with
    | Some f -> wrap_user_fn (fun () -> f out)
    | None ->
      begin match on_unknown with
        | None -> Result.return ()
        | Some f -> wrap_user_fn (fun () -> f out)
      end

let error txt =
  let red_error = "<span color='red'><b>Error: </b></span>" in
  process "rofi" [ "-markup"; "-e"; red_error ^ txt ] |> run

let menu ?(title="Select an action") ?(on_exit=`Nothing) ?on_unknown
    ?msg ?theme ?(case=false) ?misc prompts =
  menu' title msg theme case on_exit on_unknown misc prompts

let catch_errors = function
  | Result.Ok () -> ()
  | Result.Error (`Msg err) -> error err
  | Result.Error _ -> error "Unknown error"

(* Builder functions *)
let empty_row = {label = ""; style = `None; f = None}
let default_entry ?(style=`None) label = {label; style; f = None}
let entry ?(style=`None) label f = {label; style; f = Some f}

(* Extraction / packing functions *)
let get_result ?(expected=0) f_ok f_err out =
  if out.status = expected then
    Stdlib.Result.ok (f_ok out)
  else Stdlib.Result.error (`Msg (f_err out))
let pack ?(stdout="") ?(stderr="") status = {stdout; stderr; status}
let pack_out (stdout, status) = pack ~stdout status
let get_stdout {stdout; _} = stdout
let get_stderr {stderr; _} = stderr
let just x _ = x

