open Feather
open Base

let (let*) v f = Result.bind v ~f


type 'e prompt = {
  label: string;
  style: [`Urgent | `Active | `None];
  f: unit -> ([`Unit | `String of string], 'e) Result.t;
}

let get_choice input prompts =
  List.find prompts ~f:(fun p -> String.equal p.label input)
  |> Result.of_option ~error:(`Msg "Invalid action")

let gather_style select prompts =
  List.filter_mapi prompts ~f:(fun i e ->
      if Poly.(e.style = select) then Some (Int.to_string i) else None
    )
  |> String.concat ~sep:","

let menu' title msg theme on_none misc prompts f =
  (* Concat choice\nchoice\nchoice... *)
  let input = prompts |> List.map ~f:(fun e -> e.label) |> String.concat ~sep:"\n" in
  (* Concat 1,3,4,... for urgent prompts *)
  let urgents = gather_style `Urgent prompts in
  let active = gather_style `Active prompts in
  let theme_l = match theme with None -> [] | Some t -> [ "-theme"; t ] in
  let msg_l = match msg with None -> [] | Some m -> [ "-mesg"; m ] in
  let misc = match misc with None -> [] | Some m -> m in
  let choice =
    process "echo" [ "-e"; input ] |.
    process "rofi"
      (theme_l @ msg_l @ misc @ [ "-markup-rows"; "-dmenu"; "-u"; urgents ;
                                  "-a"; active; "-p"; title ])
    |> collect_stdout in
  if String.equal choice "" then
    match on_none with
    | `Nothing -> Result.return `Unit
    | `Error -> Result.fail (`Msg "No action selected")
  else begin
    match f with
    | None ->
      let* choice = get_choice choice prompts in
      choice.f ()
    | Some f ->f choice
  end

let error txt =
  let red_error = "<span color='red'><b>Error: </b></span>" in
  process "rofi" [ "-markup"; "-e"; red_error ^ txt ] |> run

let menu ?(title="Select an action") ?msg ?theme ?(on_none=`Nothing) ?misc ?f prompts =
  match menu' title msg theme on_none misc prompts f with
  | Result.Ok _ -> ()
  | Result.Error (`Msg err) -> error err
  | Result.Error _ -> error "Unknown error"

let menu_get ?(title="Select an action") ?msg ?theme ?(on_none=`Nothing) ?misc ?f prompts =
  menu' title msg theme on_none misc prompts f