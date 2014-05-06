open Definition
open Registry
open Constant
open Util
open BotUtil

(** Give your bot a 2-20 character name. *)
let name = "awesomeBot"

let stage = ref 0 
(* the resources we care about the most in this stage. Update it whenever you switch stages  *)
let resources_in_interest = ref []

module Bot = functor (S : Soul) -> struct
  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () = ()

  (* Invalid moves are overridden in game *)
  let handle_request ((b,p,t,n) : state) : move =
    let (c, r) = n in
    match r with
      | InitialRequest -> InitialMove(0, 0)
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

