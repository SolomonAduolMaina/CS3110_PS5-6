(** Constant value definitions for game logic *)

open Definition

(******************************************************************************)
(** {2 Map Constants}                                                         *)
(******************************************************************************)

let cMIN_PIECE_NUM = 0
let cMAX_PIECE_NUM = 18
let cNUM_PIECES = 19

let cMIN_POINT_NUM = 0
let cMAX_POINT_NUM = 53
let cNUM_POINTS = 54


(******************************************************************************)
(** {2 Game Constants}                                                        *)
(******************************************************************************)

(* Number of points a player needs to win *)
let cWIN_CONDITION = 10

(* Dice roll that activates the robber *)
let cROBBER_ROLL = 7

(* Max. number of trades a player can make per turn *)
let cNUM_TRADES_PER_TURN = 7

(* Max. hand size a player can have before needing to discard when the
 * robber is activated *)
let cMAX_HAND_SIZE = 7

(* Default maritime trade ratio for a port that trades any resource *)
let cMARITIME_DEFAULT_RATIO = 4

(* Num. resources generated per town *)
let cRESOURCES_GENERATED_TOWN = 1
(* Num. resources generated per city *)
let cRESOURCES_GENERATED_CITY = 2

(* Cost to build the four build types *)
let cCOST_ROAD : cost = (1,0,0,0,1)
let cCOST_TOWN : cost = (1,1,0,1,1)
let cCOST_CITY : cost = (0,0,3,2,0)
let cCOST_CARD : cost = (0,1,1,1,0)

(* Max. num. of structures a player can have on the board *)
let cMAX_TOWNS_PER_PLAYER = 5
let cMAX_CITIES_PER_PLAYER = 4
let cMAX_ROADS_PER_PLAYER = 15

(* Minimum num. of knights a player must play to be awarded largest army *)
let cMIN_LARGEST_ARMY = 3
(* Minimum length of road a player must have to be awarded longest road *)
let cMIN_LONGEST_ROAD = 5

(* Victory points scored *)
let cVP_CARD = 1 (* per victory card *)
let cVP_TOWN = 1 (* per town *)
let cVP_CITY = 2 (* per city *)
let cVP_LONGEST_ROAD = 2 (* for owning longest road trophy *)
let cVP_LARGEST_ARMY = 2 (* for owning largest army trophy *)

(* Number of cards in initial deck *)
let cNUM_KNIGHT = 14
let cNUM_VICTORYPOINT = 5
let cNUM_ROADBUILDING = 2
let cNUM_YEAROFPLENTY = 2
let cNUM_MONOPOLY = 2


(******************************************************************************)
(** {2 Play Mechanics}                                                        *)
(******************************************************************************)

let cNUM_PLAYERS = 4
let cSTEP_TIME = 0.7
let cMOVE_TIME = 3
let cMAX_TURNS = 400
let cTIMEOUT_MOVE = Action(EndTurn)


(******************************************************************************)
(** {2 New Game Defaults (Beginner Board)}                                    *)
(******************************************************************************)

let cDEFAULT_HEXES : hex list = 
          [(Forest, 11); (Pasture, 12); (Field, 9);
      (Hill, 4); (Mountain, 6); (Hill, 5); (Pasture, 10);
(Desert, 0); (Forest, 3); (Field, 11); (Forest, 4); (Field, 8);
      (Hill, 8); (Pasture, 10); (Pasture, 9); (Mountain, 3); 
           (Mountain, 5); (Field, 2); (Forest, 6)]
           
let cDEFAULT_PORTS : port list = 
  [((0, 1), 3, Any);
  ((3, 4), 2, PortResource(Wool));
  ((14, 15), 3, Any);
  ((7, 17), 2, PortResource(Ore));
  ((26, 37), 3, Any);
  ((28, 38), 2, PortResource(Grain));
  ((45, 46), 2, PortResource(Brick));
  ((47, 48), 3, Any);
  ((50, 51), 2, PortResource(Lumber))]
    
let cDEFAULT_INTERSECTIONS : intersection list =
  let rec none_list lst n =
    if n = 0 then lst else none_list (None::lst) (n-1) in
  none_list [] cNUM_POINTS

let cDEFAULT_ROADS : road list = []
let cDEFAULT_MAP : map = (cDEFAULT_HEXES, cDEFAULT_PORTS)
let cDEFAULT_STRUCTURES : structures = (cDEFAULT_INTERSECTIONS, cDEFAULT_ROADS)
let cDEFAULT_DECK : cards =
  let set = [(cNUM_KNIGHT, Knight);
    (cNUM_VICTORYPOINT, VictoryPoint);
    (cNUM_ROADBUILDING, RoadBuilding);
    (cNUM_YEAROFPLENTY, YearOfPlenty);
    (cNUM_MONOPOLY, Monopoly)] in
  (* add card n times to lst *)
  let rec add_card lst card n = if n = 0 then lst else add_card (card::lst) card (n-1) in
  let cards = List.fold_left (fun lst (n, c) -> add_card lst c n) [] set in
    Reveal(cards)

let cDEFAULT_DISCARD : discard = []
let cDEFAULT_ROBBER : robber = 7
let cDEFAULT_BOARD : board =
  (cDEFAULT_MAP, cDEFAULT_STRUCTURES, cDEFAULT_DECK, cDEFAULT_DISCARD, cDEFAULT_ROBBER)

let cDEFAULT_HAND : hand = ((0,0,0,0,0), Reveal([]))
let cDEFAULT_TROPHIES : trophies = (0, false, false)

let cDEFAULT_COLORS : color list = [Blue; Red; Orange; White]

let cDEFAULT_PLAYERS : player list = 
  List.map (fun c -> (c, cDEFAULT_HAND, cDEFAULT_TROPHIES)) cDEFAULT_COLORS
