open Definition
  
open Constant
  
open Util
  
open Print
  
open MyUtil
  
open Util2
  
let handle : state -> (resource * resource) -> state =
  fun (((board, plist, turn, (colour, request)) as s)) (have, want) ->
    let ((c, (inv, hand), (ks, lr, la)), rest) = get_player colour plist in
    let number = num_resource_in_inventory inv have
    in
		(* TODO: Handle case of port ratio *)
      match number >= cMARITIME_DEFAULT_RATIO with
      | true ->
          let minus =
            (match have with
             | Brick -> subtract_resources inv (4, 0, 0, 0, 0)
             | Wool -> subtract_resources inv (0, 4, 0, 0, 0)
             | Ore -> subtract_resources inv (0, 0, 4, 0, 0)
             | Grain -> subtract_resources inv (0, 0, 0, 4, 0)
             | Lumber -> subtract_resources inv (0, 0, 0, 0, 4)) in
          let newinv =
            (match want with
             | Brick -> plus_resources minus (1, 0, 0, 0, 0)
             | Wool -> plus_resources minus (0, 1, 0, 0, 0)
             | Ore -> plus_resources minus (0, 0, 1, 0, 0)
             | Grain -> plus_resources minus (0, 0, 0, 1, 0)
             | Lumber -> plus_resources minus (0, 0, 0, 0, 1)) in
          let p = (c, (newinv, hand), (ks, lr, la))
          in (board, (p :: rest), turn, (colour, ActionRequest))
      | false -> HandleEndTurn.handle s
  
