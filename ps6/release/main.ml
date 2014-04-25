(** Command line entry point. You don't need to worry about this. *)

(* Provide bots as dependencies *)
open Bots

let main =
  let args = Sys.argv in
  let num_args = (Array.length args) - 1 in
  if num_args < 4 then
    print_endline ("Usage: " ^ args.(0) ^ " bot1 bot2 bot3 bot4 [n] [disp]" ^
      "\n\t\t\t\t       disp = sleep | nosleep | step")
  else
    let b1 = Registry.bot_of_name args.(1) in
    let b2 = Registry.bot_of_name args.(2) in
    let b3 = Registry.bot_of_name args.(3) in
    let b4 = Registry.bot_of_name args.(4) in
    let _ = Print.set_names (args.(1), args.(2), args.(3), args.(4)) in
    if num_args = 4 then
      ignore(Play.play_game b1 b2 b3 b4)
    else if num_args >= 5 then
      let n = int_of_string args.(5) in
      let _ = 
        if num_args >= 6 then
          match args.(6) with
            | "sleep" -> Play.toggle_sleep(Play.Sleep)
            | "step" -> Play.toggle_sleep(Play.Step)
            | "nosleep" -> begin
                Print.toggle_print(false);
                Play.toggle_sleep(Play.NoSleep)
              end
            | _ -> ()
      in
        ignore(Play.bestof n b1 b2 b3 b4)
