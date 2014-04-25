open Definition

(** This is your custom state representation.
    Make sure you have encoded *at least* all of the information in state 
    such that all of the information in game is derivable
    from information in state and vice versa.
    Any information not derivable from state will likely
    be wiped out from conversions.  *)
type game


(** Convert from your state representation. *)
val state_of_game : game -> state
(** Convert to your state representation. *)
val game_of_state : state -> game


(** Create a brand new initial game state. This is non-deterministic.
    You may change this to experiment with alternate generators, such as 
    gen_random_initial_state, to test your bot's decision making
    in varied circumstances.  *)
val init_game : unit -> game


(** handle_move g m applies the next-anticipated move, m, to the game, g.
    If m is an invalid move in context, a random minimum viable move 
    is chosen instead. This will produce the outcome, which indicates whether
    someone has won, and the updated game after the move has taken place.  *)
val handle_move : game -> move -> game outcome


(** presentation g modifies a game instance g such that passing it
    to the next-expected player will not reveal any information that
    should not be accessable to that player. *)
val presentation : game -> game
