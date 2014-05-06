open Definition
open Registry
open Constant
open Util
open BotUtil

(** Give your bot a 2-20 character name. *)
let name = "awesome_bot"

let stage = ref 0 
(* the resources we care about the most in this stage. Update it whenever you switch stages  *)
let resources_in_interest = ref []
let point_pieces = Hashtbl.create cNUM_POINTS
let piece_hex = Hashtbl.create cNUM_PIECES
let first_move = ref true

let handle_InitialRequest (((map, structures, _, _, _),_,_,_) : state) : move = 
(*   if !first_move then begin *)
    let () = first_move:=false in
    let () = populate_piece_hex_hashtable piece_hex (fst map) in
    let options = get_first_town_options point_pieces piece_hex in
    let (p1,_) = List.find (fun (p,_) -> is_valid_town (fst structures) p) options in
    let () = List.fold_left (fun () (p,(_,sum)) -> print_endline ("(" ^ string_of_int p ^ ", " ^ string_of_float (sum*.36.) ^ ") ")) () options in 
    InitialMove (p1, get_some (pick_random (adjacent_points p1)))
(*   end
  else begin
    InitialMove (0, 0)
  end *)



module Bot = functor (S : Soul) -> struct
  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () =
    let () = first_move := true in
    let () = stage := 0 in
    let (b,w,o,g,l) = cCOST_CITY in 
    let lst = List.combine [b;w;o;g;l] [Brick; Wool; Ore; Grain; Lumber] in
    let () = resources_in_interest := 
      List.fold_left (fun acc (i, t) -> if i>0 then t::acc else acc) [] lst in
    let () = Hashtbl.reset point_pieces in
    let () = populate_point_pieces_hashtable point_pieces in 
    let () = Hashtbl.reset piece_hex in
    ()


  (* Invalid moves are overridden in game *)
  let handle_request ((b,p,t,(c, r)) as s : state) : move = 
    match r with
      | InitialRequest -> handle_InitialRequest s
      | RobberRequest -> RobberMove(Random.int cNUM_PIECES, Some (random_color()))
      | DiscardRequest-> DiscardMove(0,0,0,0,0)
      | TradeRequest -> TradeResponse(true)
      | ActionRequest -> 
        if is_none t.dicerolled then Action(RollDice) else Action(EndTurn)
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))


(* stages refer to highest priority. if you can't meet this priority, then try to do something else. E.g. in stage 0,
 if you can't upgrade a town to a city, then try and build a road   
stage 0: try to build another two cities towns to cities.
stage 1: build roads to get longest road trophy
stage 2: build an extra town and make it to a city and buying cards. which one is possible at that point   *)

(* should have a function that updates the stage. I should be called every time we build a city or a road. 
  it checks if we met the goal of this stage, if so, then update the stage ref cell *)

(* initialMove
first town should be near to a 6 or 8 tile, has 3 adjacent tiles,
 those three adjacent tiles are of more probable rolls (eg. 8=6>9=5>10=4>11=3>12=2).
 and it should be next to tiles with road building resources as much as possible 

 second town: should be next to a port with good roll number and a trading discount ratio for 
the resources that we have too much of from first town but we don't need for building a road. *)

(* RobberRequest: first thing make sure that you don't move the robber to a tile next to our town or city
next steal from someone who has more points than the other 2 players. calculate winning points by checking towns and cities and trophies.
do this calculation for all the three players, and sort by total points. If there is no equality on the maximum, then pick the guy with 
the maximum. If there is equality between two top players, then pick the guy with the most hidden cards (more probable to have victory point 
cards).once you pick a guy, go look for a tile that boards his settlements, and place the robber there. Make sure that the tile you pick
 at the end is not a tile next to our settlements. If you can't find any tile where this condition is satisfied, then pick the next 
 highest player and do the same again.  
*)

(* Discard request: look in the resources_in_interest list and try to keep as much as you can of those and discard the other kinds of resources *)

(* TradeRequest : have two functions enable_to_bulid() and is_fair()
enable_to_bulid re turns the bool value of "this trades will allow us to build something (a town/city or a road)"
is_fair() will return an int that's equal to [resources we get] - [resources we give]. If is_fair >=-1 then the trade is fair.

so the logic for accepting a trade request is 
match enable_to_bulid(), is_fair () with 
| false, _ -> reject 
| true, fair -> accept
| true, not fair -> reject  *)

