open Definition
  
open Constant
  
open Util
  
open Print
  
open MyUtil
  
let handle : state -> trade -> (state * bool) =
  fun (((board, plist, t, (colour, _)) as s)) (requestee, c1, c2) ->
    let (p1, _) = get_player t.active plist in
    let (p2, _) = get_player requestee plist in
    let is_active = t.active = colour in
    let non_trivial = t.active <> requestee in
    let allowed = t.tradesmade < cNUM_TRADES_PER_TURN in
    let f bool n = bool || (n > 0) in
    let not_zero = (cost_fold f false c1) && (cost_fold f false c2) in
    let enough = (has_enough_resources p1 c1) && (has_enough_resources p2 c2)
    in
      match (is_active, non_trivial, allowed, not_zero, enough) with
      | (true, true, true, true, true) ->
          let new_turn =
            {
              active = t.active;
              dicerolled = t.dicerolled;
              cardplayed = t.cardplayed;
              cardsbought = t.cardsbought;
              tradesmade = t.tradesmade + 1;
              pendingtrade = Some (requestee, c1, c2);
            }
          in ((board, plist, new_turn, (requestee, TradeRequest)), true)
      | _ -> ((HandleEndTurn.handle s), false)
  
