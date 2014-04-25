(******************************************************************************)
(** {2 Game Representation}                                                   *)
(******************************************************************************)


(** This is the forward cyclic order of player turns *)
type color = Blue | Red | Orange | White

(** The six terrain types on the board hexes. Each produces a
    corresponding resource when the hex's dice number is rolled. *)
type terrain = Hill | Pasture | Mountain | Field | Forest | Desert

(** The resource types produced by the above terrains, respectively.
    Desert does not produce any resource *)
type resource = Brick | Wool | Ore | Grain | Lumber

(** The types of development cards available in the game. *)
type card = Knight | VictoryPoint | RoadBuilding | YearOfPlenty | Monopoly

(** Dice roll values. Valid for 2 - 12 inclusive. *)
type roll = int
(** Hex list index. Valid for 0-18 inclusive. *)
type piece = int
(** Point list index. Valid for 0 - 53 inclusive. *)
type point = int
(** Maritime trade ratio. Valid for 2, 3, or 4 *)
type ratio = int

(** Valid for two adjacent points. [(p1, p2)] is line-equal to [(p2, p1)] *)
type line = point * point
(** One tile on the map. roll is the dice number that will produce resources *)
type hex = terrain * roll

(** In resource enumerated order (B,W,O,G,L) *)
type cost = int * int * int * int * int
(** Number of each resource a player has available in B,W,O,G,L order *)
type inventory = cost

(** All card lists in the game are encoded in this type so that 
    players cannot look at cards they shouldn't be able to access *)
type cards = Hidden of int | Reveal of card list
(** Represents a player's hand *)
type hand = inventory * cards


(******************************************************************************)
(** {2 TROPHIES: properties of individual players}                            *)
(******************************************************************************)

(** Number of knights the player has played thus far *)
type knights = int
(** True if the player currently has the longest road *)
type longestroad = bool
(** True if the player currently has the largest army (most knights played) *)
type largestarmy = bool
(** The three trophies the player can own (property of an individ. player *)
type trophies = knights * longestroad * largestarmy

(** Player's color, cards they own, and trophies they have. One of four "big"
    components in the overall game state.*)
type player = color * hand * trophies


(** Ports can either offer a trade ratio for a specific resource,
    or can offer a generic ratio for any resource *)
type portresource = Any | PortResource of resource
(** A port provides a specific trade ratio (e.g. ratio 4 means a 4:1 trade)
    and services the two points along its line. *)
type port = line * ratio * portresource

(** Settlement type. *)
type settlement = Town | City

(** [Some (c,s)] if the settlement [s] of color [c] exists on the board.
    Each intersection is indexed by a point *)
type intersection = (color * settlement) option
(** A road built by a player of a specific color,
    across a specific line on the board *)
type road = color * line

(** The static map: 19 tiles, 9 ports standard. Lists are in the indexed order
    given by the game board in the writeup. *)
type map = hex list * port list
(** The variable board: 53 intersections standard, 0 initial roads *)
type structures = intersection list * road list
(** Deck of cards not yet drawn. *)
type deck = cards
(** Discard pile of cards played in discarded order. *)
type discard = card list
(** The hex currently occupied by the robber. The robber is always on a piece *)
type robber = piece

(** Keeps track of everything on the physical gameboard. One of four "big"
    components of the overall game state. *)
type board = map * structures * deck * discard * robber

(** [(id * cost1 * cost2)] is a (proposed) exchange of [cost1] from active
    for [cost2] with [id]. trades can only be initiated by the active player
    as part of their turn. *)
type trade = color * cost * cost

(** Current turn state keeps track of what has happened during the turn
    and is renewed when a new turn begins *)
type turn = {
  active : color;             (** The color of the active player *)
  dicerolled : roll option;   (** [Some roll] if dice were rolled this turn *)
  cardplayed : bool;
  cardsbought : cards;        (** Any cards a player buys during their turn are
                                  stored in turn.  cardsbought and are only
                                  transfered to the player's hand at the END of
                                  the turn *)
  tradesmade : int;           (** Number of trades made this turn *)
  pendingtrade : trade option (** A trade that's just been proposed, but not yet
                                  accepted/rejected.  *)
}

(** Five different move types that the game can request a player to make *)
type request = InitialRequest
             | RobberRequest
             | DiscardRequest
             | TradeRequest
             | ActionRequest

(** Color of the next player to be prompted * type of move requested
    this is updated after every move, for the following move.  *)
type next = color * request

(** ALL the information needed to represent the current game state **)
type state = board * player list * turn * next


(******************************************************************************)
(** {2 Moves}                                                                 *)
(******************************************************************************)

(** Resource type sold * resource type bought *)
type maritimetrade = resource * resource

(** Can only build on your turn *)
type build = BuildRoad of road
           | BuildTown of point
           | BuildCity of point
           | BuildCard

(** The hex the robber will move to, and color of an adjacent player to rob,
    if one is available *)
type robbermove = piece * color option

(** RoadBuilding, YearOfPlenty is also valid with only one road, resource *)
type playcard = PlayKnight of robbermove
              | PlayRoadBuilding of road * road option
              | PlayYearOfPlenty of resource * resource option
              | PlayMonopoly of resource


(** The types of actions a player can perform in response to an ActionRequest *)
type action = RollDice
            | MaritimeTrade of maritimetrade
            | DomesticTrade of trade
            | BuyBuild of build
            | PlayCard of playcard
            | EndTurn

(** The move types a player can make, in response to InitialRequest,
    RobberRequest, DiscardRequest, TradeRequest, and ActionRequest, respectively *)
type move = InitialMove of line (** [(p1 * p2)] as line places a town at [p1] and a road from [p1] to [p2] *)
          | RobberMove of robbermove
          | DiscardMove of cost (** Number of each resource the player wishes to discard, in B,W,O,G,L order *)
          | TradeResponse of bool (** true to accept the trade, false to reject. *)
          | Action of action


(******************************************************************************)
(** {2 Winning}                                                               *)
(******************************************************************************)

(** [Some c] if player [c] has won; None if game is still in progress *)
type 'a outcome = color option * 'a

