open Definition
  
open Registry
  
open Constant
  
open Util
  
open BotUtil
  
let enables_to_build : cost -> int -> bool =
  fun inv stage ->
  let cost = match stage with
    | 0 -> cCOST_CITY
    | 1 -> cCOST_ROAD
    | _ -> cCOST_TOWN
  in 
    let (r1,r2,r3,r4,r5) = subtract_resources inv cost in
    r1>=0 && r2>=0 && r3>=0 && r4>=0 && r5>=0
  
let handle : state -> int -> move =
  fun (board, plist, turn, (colour, _)) stage ->
    let ((c, (inv, hand), (ks, lr, la)), l) = get_player colour plist in
    let (_, c1, c2) = get_some turn.pendingtrade in
    let after = plus_resources c1 (subtract_resources inv c2) in
    let fair = ((sum_cost inv) - (sum_cost after)) >= (-1)
    in
      if (enables_to_build after stage) && fair
      then TradeResponse true else TradeResponse false
  
