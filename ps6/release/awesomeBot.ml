open Definition
open Registry
open Constant
open Util
open BotUtil

(** Give your bot a 2-20 character name. *)
let name = "awesome_bot"

let stage = ref 0 
(* the resources we care about the most in this stage. Update it whenever  *)
(* you switch stages                                                       *)
let resources_in_interest = ref []
let point_pieces = Hashtbl.create cNUM_POINTS
let piece_hex = Hashtbl.create cNUM_PIECES
let first_move = ref true
let myColor = ref White


(* update resources_in_interest to include the types of resources needed for cost  *)
let update_resources_in_interest cost = 
  let (b, w, o, g, l) = cost in 
  let lst = List.combine [b; w; o; g; l] [Brick; Wool; Ore; Grain; Lumber] in
    resources_in_interest := 
      List.fold_left (fun acc (i, t) -> if i > 0 then t:: acc else acc) [] lst

(* takes player list and intersection list. Checks whether the goal of 
   the current stage is met, and updates the value of stage and 
   resources_in_interest if necessary.  *)
let update_stage_and_resources_in_interest player_list inter_list = 
  let ((_, _, (_,longestroad, _)),_) = get_player (!myColor) player_list in
  let two_cities = num_settlements (!myColor) City inter_list = 2 in
  match (!stage), longestroad, two_cities with
  | 0, _, true  -> stage := 1; update_resources_in_interest cCOST_ROAD
  | 0, _, false -> ()
  | 1, true, _  -> stage := 2; update_resources_in_interest cCOST_TOWN
  | 1, false, _ -> ()
  | 2, true, _  -> ()
  | 2, false, _ -> stage := 1; update_resources_in_interest cCOST_ROAD
  | _,_,_ -> ()

let handle_InitialRequest (((map, structures, _, _, _), _, _, (c, _)) : state) : move = 
  if !first_move then begin
    let () = first_move:= false in
    let () = myColor := c in
    let () = populate_piece_hex_hashtable piece_hex (fst map) in
    let options = get_first_town_options point_pieces piece_hex in
    try 
      let (p1, _) = List.find (fun (p, _) -> is_valid_town (fst structures) p) options in
      InitialMove (p1, get_some (pick_random (adjacent_points p1)))
    with _ -> InitialMove (Random.int cNUM_POINTS, Random.int cNUM_POINTS)
  end
  else begin
    let options = get_second_town_options point_pieces piece_hex (snd map) in
    let () = List.fold_left (fun () (p, (_, sum)) -> print_endline ("####(" ^ string_of_int p ^ ", " ^ string_of_float (sum *.36.) ^ ") ")) () options in 
    try
      let (p1, _) = List.find (fun (p, _) -> is_valid_town (fst structures) p) options in
      InitialMove (p1, get_some (pick_random (adjacent_points p1)))
    with _ -> InitialMove (Random.int cNUM_POINTS, Random.int cNUM_POINTS)
  end

let handle_RobberRequest (((map, structures, _, _, robber), pl, _, _) : state) : move =
  let options = get_opponents_vpoints pl (!myColor) (fst structures) in 
  let rec helper = function
    | [] -> None
    | (color, (vp, hidden_num, settl)):: t -> begin 
      let pieces = List.flatten (List.map (adjacent_pieces) settl) in
      let f p = p <> robber && not (has_settlement_around_piece p (!myColor) (fst structures)) in
      try
        Some (List.find f pieces, Some color)
      with _ -> helper t
    end
  in
  match helper options with 
  | Some x -> RobberMove x
  | None -> begin 
    let rec get_random_piece () =
      let p = Random.int cNUM_PIECES in 
      if p <> robber && not (has_settlement_around_piece p (!myColor) (fst structures))
      then RobberMove (p, None)
      else get_random_piece () 
    in get_random_piece ()
  end 

let handle_DiscradRequest (((_, _, _, _, _), pl, _, _) : state) : move =
  let ((_, ((b, w, o, g, l)as inv, _), _), _) = get_player (!myColor) pl in 
  let half = (sum_cost inv) / 2 in 
  let rec f out (count, kind) = 
    if count <= 0 then out else f (kind::out) (count -1, kind) 
  in
  let all = 
    List.flatten (List.map (f []) (List.combine [b; w; o; g; l] [Brick; Wool; Ore; Grain; Lumber]))
  in
  let l1, l2 = 
    List.partition (fun x -> List.mem x (!resources_in_interest)) all 
  in
  let in_intereset, not_in_interest = (randomize l1, randomize l2) in 
  let cost = 
    match in_intereset, not_in_interest with 
    | _,_ -> (half,0,0,0,0) (* TODO*)
  in
    DiscardMove cost

module Bot = functor (S : Soul) -> struct
	(* If you use side effects, start/reset your bot for a new game *)
  let initialize () =
    let () = first_move := true in
    let () = stage := 0 in
    let () = update_resources_in_interest cCOST_CITY in
    let () = Hashtbl.reset point_pieces in
    let () = populate_point_pieces_hashtable point_pieces in 
    let () = Hashtbl.reset piece_hex in
    ()


	(* Invalid moves are overridden in game *)
  let handle_request ((b, p, t, (c, r)) as s : state) : move = 
    match r with
      | InitialRequest -> handle_InitialRequest s
      | RobberRequest -> handle_RobberRequest s
      | DiscardRequest -> DiscardMove(0,0,0,0,0)
      | TradeRequest -> TradeRequestBot.handle s (!stage)
      | ActionRequest -> ActionRequestBot.handle s (!stage)
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))


(* stages refer to highest priority. if you can't meet this priority, then *)
(* try to do something else. E.g. in stage 0, if you can't upgrade a town  *)
(* to a city, then try and build a road stage 0: try to build another two  *)
(* cities towns to cities. stage 1: build roads to get longest road trophy *)
(* stage 2: build an extra town. stage 3: convert third town to city.      *)
(* stage 4: buy cards (largest army?).                                     *)

(* should have a function that updates the stage. I should be called every *)
(* time we build a city or a road. it checks if we met the goal of this    *)
(* stage, if so, then update the stage ref cell                            *)

(* initialMove first town should be near to a 6 or 8 tile, has 3 adjacent  *)
(* tiles, those three adjacent tiles are of more probable rolls (eg.       *)
(* 8=6>9=5>10=4>11=3>12=2). and it should be next to tiles with road       *)
(* building resources as much as possible second town: should be next to a *)
(* port with good roll number and a trading discount ratio for the         *)
(* resources that we have too much of from first town but we don't need    *)
(* for building a road.                                                    *)

(* RobberRequest: first thing make sure that you don't move the robber to  *)
(* a tile next to our town or city next steal from someone who has more    *)
(* points than the other 2 players. calculate winning points by checking   *)
(* towns and cities and trophies. do this calculation for all the three    *)
(* players, and sort by total points. If there is no equality on the       *)
(* maximum, then pick the guy with the maximum. If there is equality       *)
(* between two top players, then pick the guy with the most hidden cards   *)
(* (more probable to have victory point cards).once you pick a guy, go     *)
(* look for a tile that boards his settlements, and place the robber       *)
(* there. Make sure that the tile you pick at the end is not a tile next   *)
(* to our settlements. If you can't find any tile where this condition is  *)
(* satisfied, then pick the next highest player and do the same again.     *)

(* Discard request: look in the resources_in_interest list and try to keep *)
(* as much as you can of those and discard the other kinds of resources    *)

(* TradeRequest : have two functions enable_to_bulid() and is_fair()       *)
(* enable_to_bulid re turns the bool value of "this trades will allow us   *)
(* to build something (a town/city or a road)" is_fair() will return an    *)
(* int that's equal to [resources we get] - [resources we give]. If        *)
(* is_fair >=-1 then the trade is fair. so the logic for accepting a trade *)
(* request is match enable_to_bulid(), is_fair () with | false, _ ->       *)
(* reject | true, fair -> accept | true, not fair -> reject                *)

