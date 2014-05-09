open Definition
open Registry
open Constant
open Util
open BotUtil
open Print
(** Give your bot a 2-20 character name. *)
let name = "awesomebot"

module Bot = functor (S : Soul) -> struct
        (* If you use side effects, start/reset your bot for a new game *)

(* stages refer to highest priority. If you can't meet this priority, then *)
(* try to do something else. E.g. in stage 0, if you can't upgrade a town  *)
(* to a city, then try and build a road. stage 0: convert 2 towns to 2     *)
(* cities. stage 1: build roads to get longest road trophy stage 2: build  *)
(* an extra town. stage 3: convert third town to city. stage 4: buy cards  *)
(* (largest army?).                                                        *)
let stage = ref 0 
(* the resources we care about the most in this stage. Update it whenever  *)
(* you switch stages                                                       *)
let resources_in_interest = ref []
let point_pieces = Hashtbl.create cNUM_POINTS
let piece_hex = Hashtbl.create cNUM_PIECES
let first_move = ref true
let myColor = ref White
let resources_from_first_town = ref []
let goal : (point * point) option ref = ref None
let origin : (point * point) option ref = ref None

(* update resources_in_interest to include the types of resources needed   *)
(* for cost                                                                *)
let update_resources_in_interest cost = 
  let (b, w, o, g, l) = cost in 
  let lst = List.combine [b; w; o; g; l] [Brick; Wool; Ore; Grain; Lumber] in
    resources_in_interest := 
      List.fold_left (fun acc (i, t) -> if i > 0 then t:: acc else acc) [] lst

(* takes player list and intersection list. Checks whether the goal of the *)
(* current stage is met, and updates the value of stage and                *)
(* resources_in_interest if necessary.                                     *)
let update_stage_and_resources_in_interest player_list inter_list = 
(* stage 0: convert 2 towns to 2 cities. => resources_in_interest =        *)
(* cCOST_CITY stage 1: get longest road trophy. => resources_in_interest = *)
(* cCOST_ROAD stage 2: build an extra town. => resources_in_interest =     *)
(* cCOST_TOWN stage 3: convert third town to city. =>                      *)
(* resources_in_interest = cCOST_CITY stage 4: buy cards, get largest      *)
(* army. => resources_in_interest = cCOST_CARD                             *)
  let ((_, _, (_, longestroad, _)), _) = get_player (!myColor) player_list in
  let num_cities = num_settlements (!myColor) City inter_list in
    let num_towns = num_settlements (!myColor) Town inter_list in
  match (!stage)with
  | 0 -> if num_cities = 2 then (stage := 1; update_resources_in_interest cCOST_ROAD)
    else (stage := 0; update_resources_in_interest cCOST_CITY)
  | 1 -> if longestroad then (stage := 2; update_resources_in_interest cCOST_TOWN) else ()
  | 2 -> if num_towns = 1 then (stage := 3; update_resources_in_interest cCOST_CITY) else () 
  | 3 -> if num_cities = 3 then (stage := 4; update_resources_in_interest cCOST_CARD) else ()
  | _ -> ()



(* first town: should be near adjacent to 3 pieces, and those three        *)
(* adjacent pieces have high probability roll (i.e.                        *)
(* 8=6>9=5>10=4>11=3>12=2). second town: should be next to a port with     *)
(* good trading discount ratio, and border pieces with high probability    *)
(* roll. If possible, it should also be next to pieces that generate       *)
(* resources that the first town can't obtain                              *)
let rec handle_InitialRequest (((map, structures, _, _, _), _, _, (c, _)) as s : state) : move = 
  if !first_move then begin
    let () = first_move:= false in
    let () = myColor := c in
    let () = populate_piece_hex_hashtable piece_hex (fst map) in
    let options = get_first_town_options point_pieces piece_hex in
    let p = 
      (try 
        fst (List.find (fun (p, _) -> is_valid_town (fst structures) p) options )
      with _ -> Random.int cNUM_POINTS)
    in
    let lst = 
      List.map (fun piece -> resource_of_terrain (fst (Hashtbl.find piece_hex piece))) (adjacent_pieces p)
    in 
    let () = resources_from_first_town := lst in 
      InitialMove (p, get_some (pick_random (adjacent_points p)))
  end
  else begin
    let options = get_second_town_options point_pieces piece_hex (snd map) (!resources_from_first_town) 
    in
(* let () = List.fold_left (fun () (p, (_, sum)) -> print_endline ("####(" *)
(* ^ string_of_int p ^ ", " ^ string_of_float sum ^ ") ")) () options in   *)
    try
      let (p1, _) = List.find (fun (p, _) -> is_valid_town (fst structures) p) options in
      InitialMove (p1, get_some (pick_random (adjacent_points p1)))
    with _ -> begin
      first_move := true;
      handle_InitialRequest s
    end
  end

(* RobberRequest: first thing make sure that you don't move the robber to  *)
(* a piece next to our town or city. Robber should steal from the opponent *)
(* with the highest victory points at the time. calculate victory points   *)
(* by checking towns and cities and trophies. If there is no unique        *)
(* opponent with highest victory points, then pick the opponents that has  *)
(* more hidden cards. once you identify a player to steal from, go look    *)
(* for a piece that boards any of his settlements, and place the robber    *)
(* there. Make sure that the piece you pick at the end is not a piece next *)
(* to our settlements. If you can't find any piece where this condition is *)
(* satisfied, then pick the opponent with the next highest victory points  *)
(* and do the same again.                                                  *)
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

(* Discard request: look in the resources_in_interest list and try to keep *)
(* as much as you can of those and discard the other kinds of resources    *)
let handle_DiscardRequest (((_, _, _, _, _), pl, _, _) : state) : move =
  let ((_, ((b, w, o, g, l)as inv, _), _), _) = get_player (!myColor) pl in 
  let half = (sum_cost inv) / 2 in 
  let rec f out (count, kind) = 
    if count <= 0 then out else f (kind:: out) (count -1, kind) 
  in
  let all = 
    List.flatten (List.map (f []) (List.combine [b; w; o; g; l] [Brick; Wool; Ore; Grain; Lumber]))
  in
  let l1, l2 = 
    List.partition (fun x -> List.mem x (!resources_in_interest)) all 
  in
  let in_intereset, not_in_interest = (randomize l1, randomize l2) in 
  let rec helper out lst1 lst2 =
    if sum_cost out = half then out 
    else begin
      match lst1, lst2 with 
      | h:: t, _ -> helper (plus_resources out (single_resource_cost h)) t lst2
      | [], h:: t -> helper (plus_resources out (single_resource_cost h)) lst1 t 
      | _ -> (0,0,0,0,0)
    end 
  in
  let cost = helper (0,0,0,0,0) not_in_interest in_intereset
  in
    DiscardMove cost

  let initialize () =
    let () = first_move := true in
    let () = stage := 0 in
    let () = update_resources_in_interest cCOST_CITY in
    let () = Hashtbl.reset point_pieces in
    let () = populate_point_pieces_hashtable point_pieces in 
    let () = Hashtbl.reset piece_hex in
    let () = resources_from_first_town := [] in
    let () = goal := None in
        let () = origin := None in
    ()

        (* Invalid moves are overridden in game *)
  let handle_request ((b, p, t, (c, r)) as s : state) : move = 
        let (_, (inter_list, _), _, _, _) = b in
        let () = update_stage_and_resources_in_interest p inter_list in
                (* let () = print_endline ("myColor = " ^ string_of_color          *)
                (* !myColor) in                                                    *)
    match r with
      | InitialRequest -> handle_InitialRequest s
      | RobberRequest -> handle_RobberRequest s
      | DiscardRequest -> DiscardMove(0,0,0,0,0)
      | TradeRequest -> TradeRequestBot.handle s (!stage)
      | ActionRequest -> let m, (opt, orig) = ActionRequestBot.handle s (!stage) false false (!goal) (!origin) in
												let () = goal := opt in  let () = origin := orig in m 
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))