open Definition
open Constant
open Util
open Print

(* type state = board * player list * turn * next *)
type game = state

let state_of_game g = g
let game_of_state s = s

let init_game () = game_of_state (gen_initial_state())

let handle_move ((b,pl,t,(c, r)) as s : game) (m : move) : game = 
  let updated_game = match r, m with 
    | InitialRequest, InitialMove (p1,p2) -> handle_InitialMove s (p1,p2)
    | InitialRequest, _ -> handle_InitialMove s (Random.int cNUM_POINTS, Random.int cNUM_POINTS) (* invalid move *)
    
    | RobberRequest, RobberMove (p, x) -> handle_RobberMove s (p, x)
    | RobberRequest, _ -> handle_RobberMove s (Random.int cNUM_PIECES, random_color())) (* invalid move *)
    
    | DiscardRequest, DiscardMove cost -> handle_DiscardMove s cost
    | DiscardRequest, _ -> handle_DiscardMove s (0, 0, 0, 0, 0) (* invalid move: TODO: make it discard floor(.5 resources) picked at random *)
    
    | TradeRequest, TradeResponse b -> handle_TradeResponse s b
    | TradeRequest, _ -> handle_TradeResponse s (Random.int 2 = 1) (* invalid move *)
    
    | ActionRequest, Action (RollDice) -> handle_RollDice s
    | ActionRequest, Action (MaritimeTrade maritimetrade) -> handle_MaritimeTrades s maritimetrade
    | ActionRequest, Action (DomesticTrade trade) -> handle_DomesticTrade s trade
    | ActionRequest, Action (BuyBuild build) -> handle_BuyBuild s build
    | ActionRequest, Action (PlayCard playcard) -> handle_PlayCard s playcard
    | ActionRequest, Action (EndTurn) -> handle_EndTurn s
    | ActionRequest, _ -> if is_none t.dicerolled then handle_RollDice s else handle_EndTurn s
  in 
    print_update c m updated_game; (*TODO: update m if it was an invalid move*)
    (None, updated_game)

let presentation s = s



(** [(p1 * p2)] as line places a town at [p1] and a road from [p1] to [p2] *)
(* invalid move if p1 has a town already or it's at a distance of one road from another town. Also invalid if road p1-p2 already exist.
Check for valid -> add town and road.
[TO-DO: figure out how to make two initail moves as the rules states. specifically what will next be updated to]

updated game = new town and road added to the board. next will be next player * InitialRequest (TODO: figure out if everybody went twice for initial move)*)
let handle_InitialMove s (p1, p2) = s 



(* RobberMove (p, x)
re-locate the robber to piece p. if x = None then do nothing else. 
if x = Some c, then steal a resource from player c, if c has a town that borders p.
invalid move if c doesn't have a town bordering p, in which case, make x = none  

question: 
is stolen resource a 1 unit of some resource or all avaliable units of a single resource?
is the type of resource selected at random?

updated game = robber moved to p and if applicable, resources are moved from c to this player. next = same player * ActionRequest*)
let handle_RobberMove s (p, x) = s



(** Number of each resource the player wishes to discard, in B,W,O,G,L order.
invalid move 
updated game = update this player's resources. 

look at cMAX_HAND_SIZE in Constant.ml (player only discard half of their cards only if they have MORE than cMAX_HAND_SIZE )

*)

let handle_DiscardMove s cost = s



(** b = true to accept the trade | false to reject. 
invalid move if there is no offer for this current player or number of trades made in this turn = cNUM_TRADES_PER_TURN

updated game = update [turn] to move the deal from the bending trades. Apply the
 trade and update the resources of both sides of the trade if trade was accepted. 
next = same player * ActionRequest *)
let handle_TradeResponse s b =  s


(* generate a random roll between 2-12. If roll = cROBBER_ROLL, send a DiscardRequest to other players. after they all discard 
the floor of half their resources, send a RobberRequest to the active player. handle these moves and come back to the actie player.
  If rool != cROBBER_ROLL, traverse the hex list in board to find all the hex with the roll number, make those generate resources
  and update the resources of players that have neighboring towns or cities to those hexes.

  invalid if dice was rolled before in this turn (i.e. if is_none (turn.dicerolled) = false) 

  update game = recourses updated and if roll != cROBBER_ROLL and next = same player * ActionRequest,
  if cROBBER_ROLL updated game next = same player * RobberRequest after sending DiscardRequest and updating each player's resoures. 

  look at cMAX_HAND_SIZE in Constant.ml (player only discard half of their cards only if they have MORE than cMAX_HAND_SIZE )
 *)
let handle_RollDice s = s




let handle_MaritimeTrade s maritimetrade = s


let handle_DomesticTrade s trade = s


let handle_BuyBuild s build = s


let handle_PlayCard s playcard = s


let handle_EndTurn s = s