open Definition
  
open Constant
  
open Util
  
open Print
  
open MyUtil
  
let handle : state -> state =
  fun (((board, pl, t, (colour, request)) as s)) ->
    match t.dicerolled with
    | Some _ ->
        let ((c, (inv, hand), (ks, lr, la)), l) = get_player t.active pl in
        let newhand = wrap_reveal ((reveal t.cardsbought) @ (reveal hand)) in
        let p = (c, (inv, newhand), (ks, lr, la)) in
        let new_colour = next_turn colour in
        let fresh = new_turn new_colour
        in (board, (p :: l), fresh, (new_colour, ActionRequest))
    | None -> s
  
