open Definition
  
open Constant
  
open Util
  
open Print
  
open MyUtil
  
let handle : state -> trade -> state =
  fun (((board, plist, t, _) as s)) (requestee, c1, c2) ->
    let (p1, l) = get_player t.active plist in
    let (p2, _) = get_player requestee l in
    let f bool n = bool || (n > 0) in
    let not_zero = (cost_fold f false c1) && (cost_fold f false c2) in
    let enough = (has_enough_resources p1 c1) && (has_enough_resources p2 c2)
    in
      match (not_zero, enough, (t.tradesmade < cNUM_TRADES_PER_TURN)) with
      | (true, true, true) ->
          let new_turn =
            {
              active = t.active;
              dicerolled = t.dicerolled;
              cardplayed = t.cardplayed;
              cardsbought = t.cardsbought;
              tradesmade = t.tradesmade + 1;
              pendingtrade = Some (requestee, c1, c2);
            }
          in (board, plist, new_turn, (requestee, TradeRequest))
      | _ -> HandleEndTurn.handle s
  
