open Definition
  
open Constant
  
open Util
  
open Print
  
open MyUtil
  
let handle : state -> (resource * resource) -> state * bool =
  fun (((board, plist, t, _) as s)) (have, want) ->
    let ((c, (inv, hand), (ks, lr, la)), rest) = get_player t.active plist in
    let number = num_resource_in_inventory inv have in
    let ((_, ports), (insecs, _), _, _, _) = board in
    let ratio = least_ratio c ports insecs have
    in
      match number >= ratio with
      | true ->
          let minus =
            (match have with
             | Brick -> subtract_resources inv (ratio, 0, 0, 0, 0)
             | Wool -> subtract_resources inv (0, ratio, 0, 0, 0)
             | Ore -> subtract_resources inv (0, 0, ratio, 0, 0)
             | Grain -> subtract_resources inv (0, 0, 0, ratio, 0)
             | Lumber -> subtract_resources inv (0, 0, 0, 0, ratio)) in
          let newinv =
            (match want with
             | Brick -> plus_resources minus (1, 0, 0, 0, 0)
             | Wool -> plus_resources minus (0, 1, 0, 0, 0)
             | Ore -> plus_resources minus (0, 0, 1, 0, 0)
             | Grain -> plus_resources minus (0, 0, 0, 1, 0)
             | Lumber -> plus_resources minus (0, 0, 0, 0, 1)) in
          let p = (c, (newinv, hand), (ks, lr, la))
          in (board, (p :: rest), t, (t.active, ActionRequest)), true
      | false -> HandleEndTurn.handle s, false
  
