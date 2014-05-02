open Definition
open Constant
open Util
open Print
open MyUtil

(* type state = board * player list * turn * next *)
type game = state

let state_of_game g = g
let game_of_state s = s

let init_game () = game_of_state (gen_initial_state())

(* [(p1 * p2)] as line places a town at [p1] and a road from [p1] to [p2]  *)
(* invalid move if p1 has a town already or it's at a distance of one road *)
(* from another town (i.e. a neighboring points of p1 has a town). Also    *)
(* invalid if road p1-p2 already exist. If invalid, then pick a random     *)
(* valid line. updated game = add new town and road to the board. Generate *)
(* resources for the player from the new town if this is his second town   *)
(* if there are less than 4 twons on the board (including the new town     *)
(* created here) -> next = next_turn (c) * InitialRequest if there are     *)
(* exactly 4 towns (including the new town created here) (i.e. next_turn   *)
(* (c) = t.active)-> next = x * InitialRequest if there are >= 4 towns     *)
(* (including the new town created here) -> next = prev_turn (c) *         *)
(* InitialRequest if there are 8 towns (i.e. c = t.active) -> next =       *)
(* t.active * ActionRequest                                                *)
let handle_InitialMove ((((map, structures, deck, discard, robber), pl , t , (c, req))) : game) ((p1, p2) : line) =
	let (inter_list, road_list) = structures
	in
	let n = num_towns inter_list
	in
	let rec check_and_fix (x, y) =
		match is_valid_town inter_list x, is_valid_line (x, y) with
		| true, true -> (x, y)
		| true, false -> (x, get_some (pick_random (adjacent_points x)))
		| _, _ -> check_and_fix (Random.int cNUM_POINTS, Random.int cNUM_POINTS)
	in
	let (new_p1, new_p2) = check_and_fix (p1, p2)
	in
	let new_pl = if n < 4 then pl
		else add_resources_to_player c
				(gen_all_resources new_p1 cRESOURCES_GENERATED_TOWN (fst map) robber) pl
	in
	let new_structures =
		(add_settlement new_p1 c Town inter_list, add_road (new_p1, new_p2) c road_list)
	in
	let next =
		if (n +1) < 4 then (next_turn (c), InitialRequest)
		else if (n +1) = 4 then (c, InitialRequest)
		else if (n +1) > 4 && (n +1) < 8 then (prev_turn (c), InitialRequest)
		else (t.active, ActionRequest)
	in
	(((map, new_structures, deck, discard, robber), new_pl, t, next), InitialMove (new_p1, new_p2))

(* RobberMove (p, x) re-locate the robber to piece p. if x = None then do  *)
(* nothing else. if x = Some pl, then steal a resource from player pl, if  *)
(* pl has a town that borders p. invalid move if pl doesn't have a town    *)
(* bordering p, in which case, make x = none updated game = robber moved   *)
(* to p and if applicable, a unit of a random resource of player pl is     *)
(* moved from pl to t.active player. next = t.active * ActionRequest       *)
let handle_RobberMove ((map, structures, deck, discard, robber), pl, t, (c, r)) (p, x) =
	let new_pl, robbed =
		if not (is_none x) && has_settlement_around_piece p (get_some x) (fst structures)
		then (steal_from_and_give_to (get_some x) c pl, x) else (pl, None)
	in
	let new_robber = p and next = (t.active, ActionRequest) in
	(((map, structures, deck, discard, new_robber), new_pl, t, next), RobberMove (p, robbed))

(* Number of each resource the player wishes to discard, in B,W,O,G,L      *)
(* order. invalid move if the discarded is not equal to the floor of .5 of *)
(* the total. In which case, pick a random cost that equal the exact       *)
(* amount required to discard. invalid if they have <= cMAX_HAND_SIZE, in  *)
(* which case ignore the move updated game = update c's resources. let x = *)
(* c LOOP: If next_turn (x) = t.active -> next = t.active * RobberRequest  *)
(* else if next_turn (x) has > cMAX_HAND_SIZE resources -> next =          *)
(* next_turn (x) * DiscardRequest else let x = next_turn (x) and jump to   *)
(* LOOP                                                                    *)
let handle_DiscardMove ((b, pl, t, (c, r)) : game) cost =
	let (x, xs) = get_player c pl
	in
	let new_x, new_cost = check_and_fix_discard_move x cost
	in
	let new_pl = new_x :: xs
	in
	let rec next p =
		let x = next_turn (p)
		in
		if x = t.active then (t.active, RobberRequest)
		else begin
			let (y, _) = get_player x new_pl in
			if needs_to_discard y then (x, DiscardRequest)
			else next x
		end
	in
	((b, new_pl, t, next c), DiscardMove new_cost)

(* accept = true to accept the trade | false to reject. invalid move if    *)
(* any of the two players have insufficient fundings for the trade to      *)
(* happen updated game = update [turn] to move the deal from the bending   *)
(* trades. Apply the trade and update the resources of both sides of the   *)
(* trade if trade was accepted. next = same player * ActionRequest         *)
let handle_TradeResponse (b, pl, t, (c, r)) accepted =
	let new_turn =
		{ active = t.active; dicerolled = t.dicerolled;
			cardplayed = t.cardplayed; cardsbought = t.cardsbought ;
			tradesmade = t.tradesmade; pendingtrade = None }
	in
	let new_pl, new_accepted =
		if not accepted then (pl, false) else begin
			let (id, from_active, from_other) = get_some t.pendingtrade in
			let (active, _) = get_player t.active pl and (other, _ ) = get_player id pl in
			if has_enough_resources other from_other &&
			has_enough_resources active from_active
			then begin
				let change_active = subtract_resources from_other from_active
				and change_other = subtract_resources from_active from_other in
				let pl1 = add_resources_to_player t.active change_active pl in
				let pl2 = add_resources_to_player id change_other pl1 in
				(pl2, true)
			end
			else (pl, false)
		end
	in
	((b, new_pl, new_turn, (t.active, ActionRequest)), TradeResponse new_accepted)

(* generate a random roll between 2-12. If roll = cROBBER_ROLL, send a     *)
(* DiscardRequest to other players. after they all discard the floor of    *)
(* half their resources, send a RobberRequest to the active player. handle *)
(* these moves and come back to the active player. If rool !=              *)
(* cROBBER_ROLL, traverse the hex list in board to find all the hex with   *)
(* the roll number, make those generate resources and update the resources *)
(* of players that have neighboring towns or cities to those hexes.        *)
(* invalid if dice was rolled before in this turn (i.e. if is_none         *)
(* (turn.dicerolled) = false) update game = recourses updated and if roll  *)
(* != cROBBER_ROLL and next = same player * ActionRequest, if cROBBER_ROLL *)
(* updated game next = same player * RobberRequest after sending           *)
(* DiscardRequest and updating each player's resoures. look at             *)
(* cMAX_HAND_SIZE in Constant.ml (player only discard half of their cards  *)
(* only if they have MORE than cMAX_HAND_SIZE )                            *)
let handle_RollDice ((map, structures, deck, discard, robber) as b, pl, t, (c, r)) =
	let roll = random_roll () in
	let new_turn =
		{ active = t.active; dicerolled = Some roll;
			cardplayed = t.cardplayed; cardsbought = t.cardsbought ;
			tradesmade = t.tradesmade; pendingtrade = t.pendingtrade }
	in
	let rec get_next_if_ROBBER_ROLL p =
		let x = next_turn (p)
		in
		if x = t.active then (t.active, RobberRequest)
		else begin
			let (y, _) = get_player x pl in
			if needs_to_discard y then (x, DiscardRequest)
			else get_next_if_ROBBER_ROLL x
		end
	in
	let next =
		if roll = cROBBER_ROLL then get_next_if_ROBBER_ROLL t.active
		else (t.active, ActionRequest)
	in
	let new_pl =
		if roll = cROBBER_ROLL then pl
		else gen_roll_resources pl (fst structures) (fst map) roll robber
	in
	((b, new_pl, new_turn, next), Action (RollDice))

let handle : state -> state * bool -> move -> state * move =
	fun s (s', bool) move ->
			match bool with
			| true -> (s', move)
			| false -> if s <> s' then (s', Action (EndTurn)) else handle_RollDice s

let handle_MaritimeTrade s (have, want) =
	let move = Action (MaritimeTrade (have, want)) in
	handle s (HandleMaritimeTrade.handle s (have, want)) move

let handle_DomesticTrade s trade =
	let move = Action (DomesticTrade trade) in
	handle s (HandleDomesticTrade.handle s trade) move

let handle_BuyBuild s build =
	let move = Action (BuyBuild build) in
	handle s (HandleBuyBuild.handle s build) move

let handle_PlayCard s playcard =
	let move = Action (PlayCard playcard) in
	handle s (HandlePlayCard.handle s playcard) move

let handle_EndTurn s = let s' = HandleEndTurn.handle s in
	if s <> s' then (s', Action (EndTurn)) else handle_RollDice s

let handle_move ((b, pl, t, (c, r)) as s : game) (m : move) : game outcome =
	let (updated_game, actual_move) = match r, m with
		| InitialRequest, InitialMove (p1, p2) -> handle_InitialMove s (p1, p2)
		| InitialRequest, _ -> handle_InitialMove s (Random.int cNUM_POINTS, Random.int cNUM_POINTS) (* invalid move *)
		
		| RobberRequest, RobberMove (p, x) -> handle_RobberMove s (p, x)
		| RobberRequest, _ -> handle_RobberMove s (Random.int cNUM_PIECES, Some (random_color())) (* invalid move *)
		
		| DiscardRequest, DiscardMove cost -> handle_DiscardMove s cost
		| DiscardRequest, _ -> handle_DiscardMove s (0, 0, 0, 0, 0) (* invalid move: TODO: make it discard floor(.5 resources) picked at random *)
		
		| TradeRequest, TradeResponse b -> handle_TradeResponse s b
		| TradeRequest, _ -> handle_TradeResponse s (Random.int 2 = 1) (* invalid move *)
		
		| ActionRequest, Action (RollDice) -> handle_RollDice s
		| ActionRequest, Action (MaritimeTrade maritimetrade) -> handle_MaritimeTrade s maritimetrade
		| ActionRequest, Action (DomesticTrade trade) -> handle_DomesticTrade s trade
		| ActionRequest, Action (BuyBuild build) -> handle_BuyBuild s build
		| ActionRequest, Action (PlayCard playcard) -> handle_PlayCard s playcard
		| ActionRequest, Action (EndTurn) -> handle_EndTurn s
		| ActionRequest, _ -> handle_EndTurn s
	in
	print_update c actual_move updated_game; (*TODO: update m if it was an invalid move*)
	(None, updated_game)

let presentation ((board, plist, turn, (colour, r)) : game) : game =
	let f plist ((c, (inv, hand), ts) : player) =
		if colour <> c
		then (c, (inv, (hide hand)), ts) :: plist
		else (c, (inv, hand), ts) :: plist in
	let custom = List.fold_left f [] plist in
	let ((map, structures, deck, discards, robber) : board) = board in
	let newboard = (map, structures, (hide deck), discards, robber) in
	(newboard, custom, turn, (colour, r))
