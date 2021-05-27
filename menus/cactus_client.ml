open Feather
open Feather_dmenu

let (let*) = Result.bind

let get_result ok err out =
  match out.status with
  | 0 -> Result.ok ok
  | _ -> Result.error (`Msg err)

let notify txt =
  let success_icon ="/usr/share/icons/gnome/32x32/status/info.png" in
  process "notify-send"
    [ "-t"; "8000"; "Heater"; "-i"; success_icon; txt ] |> run

let send_raspi ?(hostname="raspi") ?(port=2713) command =
  let expect_file = Printf.sprintf {|
      log_user 0
      set timeout 5
      spawn telnet %s %d
      expect "> "
      send "%s\n"
      log_user 1
      expect "> "
      log_user 0
      send "exit\n"
      expect
    |} hostname port command in
  let out = echo expect_file |. process "expect" [ "-" ] |> collect stdout_lines in
  if Stdlib.(List.length out.stdout < 2) then
    Result.error (`Msg "could not reach Raspberry Pi or cactus server.")
  else
    get_result (List.nth out.stdout 1) "expect failed" out

let float_of_string_r x =
  float_of_string_opt x |> Option.to_result ~none:(`Msg "Not a float")

let read_temp _ =
  let* answer = send_raspi "read" in
  let* temp =
    echo answer |. sed {|^Temp: \(.*\)°.*$|} {|\1|}
    |> collect_stdout
    |> float_of_string_r
  in
  notify (Printf.sprintf "Current temperature: %g°C" temp) ;
  Result.ok ()

let turn mode _ =
  let* answer = send_raspi ("turn " ^ mode) in
  if answer.[0] = 'A' then Result.error (`Msg answer)
  else (
    notify answer ;
    Result.ok ()
  )

let read_goal () =
  let* answer = send_raspi "get" in
  float_of_string_r answer

let read_goal_wrap _ =
  let* goal = read_goal () in
  notify (Printf.sprintf "Current goal: %g°C" goal) ;
  Result.ok ()

let set_goal x =
  let* answer = send_raspi ("set " ^ (string_of_float x)) in
  if answer = "Goal changed" then Result.ok ()
  else Result.error (`Msg answer)

let modify step _ =
  let* current = read_goal () in
  let next = current +. step in
  let* () = set_goal next in
  notify (Printf.sprintf "Goal: %g°C -> %g°C" current next) ;
  Result.ok ()

let custom_temp _ =
  Dmenu.menu ~title:"Write a temperature" ~msg:"Expects a float"
    ~on_unknown:(fun out ->
        if String.equal "" out.stdout then failwith "LOL";
        let* temp = float_of_string_r out.stdout in
        let* _ = set_goal temp in
        notify (Printf.sprintf "Goal changed to: %g°C" temp);
        Result.ok ()
      ) []

let main =
  Dmenu.menu ~title:"Heater menu"
    ~msg:"Actions will take effect after up to one minute" Dmenu.[
        entry " Show temperature" read_temp;
        entry "什 Show goal" read_goal_wrap;
        entry ~style:`Active " More heat!" (modify 1.);
        entry ~style:`Active " Less heat..." (modify (-1.));
        entry ~style:`Active " Custom temperature" custom_temp;
        entry ~style:`Urgent "⏽ Turn on" (turn "on");
        entry ~style:`Urgent "⭘ Turn off" (turn "off");
      ] |> Dmenu.catch_errors
