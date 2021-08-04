open Feather

let bar_name = "main"

let kill_old () = process "killall" [ "-q"; "polybar" ] |> run

let current_env =
  let parse s = Scanf.sscanf s "%s@=%s" (fun a b -> (a, b)) in
  Unix.environment () |> Array.to_list
  |> List.map parse

let launch_new () =
  let launch_one mon =
    Printf.printf "Launching a bar on %s\n" mon;
    let env = ("MONITOR", mon) :: current_env in
    process "polybar" [ "--reload"; bar_name ] |> run_bg ~env
  in
  let monitors = process "polybar" [ "-m" ] |. cut ~d:':' 1 |> collect stdout |> lines in
  List.iter launch_one monitors

let _ =
  kill_old ();
  launch_new ()
