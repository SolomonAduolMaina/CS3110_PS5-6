(** Controls the process of playing a game. You don't need to worry about this *)

open Definition
open Registry
open Constant
open Util
open Print

(* What to do between moves *)
type sleep = Sleep | NoSleep | Step

let sleep_ref = ref Sleep
let toggle_sleep (s : sleep) = sleep_ref := s

let sleep (s : state) (seconds : float) : unit =
  match !sleep_ref with
    | Sleep -> ignore (Unix.select [] [] [] seconds)
    | NoSleep -> ()
    | Step -> match read_line() with
      | "q" | "quit" -> exit 0
      | "s" | "state" | "code" | "print" -> print_code_of_state s
      | _ -> ()


(* timeout secs f a b attempts to return f a in secs seconds, 
    and returns b if timeout occurs *)
exception Timeout

let timeout (time : int) (f : 'a -> 'b) (arg : 'a) (default : 'b) =
  let sigalrm_handler = Sys.Signal_handle (fun _ -> raise Timeout) in
  let old_behavior = Sys.signal Sys.sigalrm sigalrm_handler in
  let reset_sigalrm () = ignore(Unix.alarm 0); Sys.set_signal Sys.sigalrm old_behavior in
    ignore (Unix.alarm time);
    try
      let res = f arg in reset_sigalrm (); res
    with Timeout -> default


(* Play a game and return the winner's color. *)
let play_game (module B : BOT) (module R : BOT) (module O : BOT) (module W : BOT) : color option =
  let rec play ((_,_,t,(c, r)) as s : state) turns =
    let handler =
      match c with
        | Blue -> B.handle_request
        | Red -> R.handle_request
        | Orange -> O.handle_request
        | White -> W.handle_request
    in
    let game = Game.game_of_state s in
    let s = Game.state_of_game (Game.presentation game) in
    let move = timeout cMOVE_TIME handler s cTIMEOUT_MOVE in
    let (winner, game) = Game.handle_move game move in
    let s = Game.state_of_game game in
    let (_,_,turn,_) = s in
    let turns = if t.active <> turn.active && r <> InitialRequest then turns+1 else turns in
    sleep s cSTEP_TIME;
    match winner with
      | None -> if turns < cMAX_TURNS then play s turns else let _ = print_draw() in None
      | Some c -> print_winner c; Some c
  in
  let _ = B.initialize(); R.initialize(); O.initialize(); W.initialize() in
    play (Game.state_of_game (Game.init_game())) 0


(* Play n games sequentially and print out a summary of the results 
    Expedite this process with the "nosleep" option *)
let bestof n blue red orange white : unit =
  let play_games n =
    let rec iter winners i =
      if i >= n then winners else
        let winners =
          match play_game blue red orange white with
            | Some c -> c::winners
            | None -> winners
        in
          iter winners (i+1)
    in
      iter [] 0
  in
  let winners = play_games n in
  let colors = List.fold_left (fun lst x -> if List.mem x lst then lst else x::lst) [] winners in
  let counts = List.map (fun c -> List.length (List.filter ((=) c) winners)) colors in
  let draws = n - (list_sum counts) in
    print_hbar();
    List.iter2 print_count (List.map string_of_color colors) counts;
    if draws > 0 then print_count "draws" draws
