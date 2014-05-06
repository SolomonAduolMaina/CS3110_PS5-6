open Definition
  
open Registry
  
open Constant
  
open Util
  
open BotUtil
  
let enables_to_build : cost -> int -> bool =
  fun (b, w, o, g, l) stage ->
    match stage with
    | 0 -> (o >= 3) && (g >= 2)
    | 1 -> (b >= 1) && (l >= 2)
    | 2 -> (b >= 1) && ((w >= 1) && ((g >= 1) && (l >= 1)))
    | 3 -> (o >= 3) && (g >= 2)
    | _ -> (w >= 1) && ((o >= 1) && (g >= 1))
  
let handle : state -> int -> move =
  fun (board, plist, turn, (colour, _)) stage ->
    let ((c, (inv, hand), (ks, lr, la)), l) = get_player colour plist in
    let (_, c1, c2) = get_some turn.pendingtrade in
    let after = plus_resources c1 (subtract_resources inv c2) in
    let fair = ((sum_cost inv) - (sum_cost after)) >= (-1)
    in
      match ((enables_to_build after stage), fair) with
      | (true, true) -> TradeResponse true
      | _ -> TradeResponse false
  
