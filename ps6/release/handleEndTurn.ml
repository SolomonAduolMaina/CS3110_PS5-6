open Definition
  
open Constant
  
open Util
  
open Print
  
open MyUtil
  
let handle : state -> state =
  fun (((board, pl, t, _) as s)) ->
    match t.dicerolled with
    | Some _ ->
        let ((c, (inv, hand), ts), l) = get_player t.active pl in
        let newhand = wrap_reveal ((reveal t.cardsbought) @ (reveal hand)) in
        let p = (c, (inv, newhand), ts) in
        let new_colour = next_turn t.active in
        let fresh = new_turn new_colour in
        let (_, (inters, roads), _, _, _) = board in
        let new_pl = update_longest_road_trophy (p :: l) roads inters in 
        (board, new_pl, fresh, (new_colour, ActionRequest))
    | None -> s
  
