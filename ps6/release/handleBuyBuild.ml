open Definition
  
open Constant
  
open Util
  
open Print
  
open MyUtil
  
open Util2
  
let handle_road (board, plist, turn, (colour, request)) bld (c1, (p1, p2)) =
  let (p, l) = get_player colour plist in
  let (c, (inv, hand), (ks, lr, la)) = p in
  let (a1, (insecs, roads), deck, a4, a5) = board in
  let not_bought = not (road_bought (p1, p2) roads) in
  let is_valid = (is_valid_line (p1, p2)) && (c1 = c) in
  let cost = cost_of_build bld in
  let enough = has_enough_resources p cost in
  let allowed = (player_roads c roads) < cMAX_ROADS_PER_PLAYER
  in
    match (not_bought, is_valid, enough, allowed) with
    | (true, true, true, true) ->
        let newinv = subtract_resources inv cost in
        let p = (c, (newinv, hand), (ks, lr, la)) in
        let newroads = (c1, (p1, p2)) :: roads in
        let b = (a1, (insecs, newroads), deck, a4, a5)
        in (b, (p :: l), turn, (colour, ActionRequest))
    | _ -> HandleEndTurn.handle (board, plist, turn, (colour, request))
  
let handle_town (board, plist, turn, (colour, request)) build point =
  let (p, l) = get_player colour plist in
  let (c, (inv, hand), (ks, lr, la)) = p in
  let (a1, (insecs, roads), deck, a4, a5) = board in
  let not_built = is_none (List.nth insecs point) in
  let is_valid = is_valid_town insecs point in
  let cost = cost_of_build build in
  let enough = has_enough_resources p cost in
  let player_towns = player_settlements c Town insecs in
  let allowed = player_towns < cMAX_TOWNS_PER_PLAYER
  in
    match (not_built, is_valid, enough, allowed) with
    | (true, true, true, true) ->
        let newinv = subtract_resources inv cost in
        let p = (c, (newinv, hand), (ks, lr, la)) in
        let newins = add_settlement point c Town insecs in
        let b = (a1, (newins, roads), deck, a4, a5)
        in (b, (p :: l), turn, (colour, ActionRequest))
    | _ -> HandleEndTurn.handle (board, plist, turn, (colour, request))
  
let handle_city (board, plist, turn, (colour, request)) build point =
  let (p, l) = get_player colour plist in
  let (c, (inv, hand), (ks, lr, la)) = p in
  let (a1, (insecs, roads), deck, a4, a5) = board in
  let settlement = List.nth insecs point in
  let built = not (is_none settlement) in
  let (c', setl) = get_some settlement in (* Danger *)
  let is_valid = (c = c') && (setl = Town) in
  let cost = cost_of_build build in
  let enough = has_enough_resources p cost in
  let player_cities = player_settlements c City insecs in
  let allowed = player_cities < cMAX_CITIES_PER_PLAYER
  in
    match (built, is_valid, enough, allowed) with
    | (true, true, true, true) ->
        let newinv = subtract_resources inv cost in
        let p = (c, (newinv, hand), (ks, lr, la)) in
        let newins = add_settlement point c City insecs in
        let b = (a1, (newins, roads), deck, a4, a5)
        in (b, (p :: l), turn, (colour, ActionRequest))
    | _ -> HandleEndTurn.handle (board, plist, turn, (colour, request))
  
let handle_card (((board, plist, turn, (colour, request)) as s)) build =
  let (p, l) = get_player colour plist in
  let (c, (inv, hand), (ks, lr, la)) = p in
  let (a1, (insecs, roads), deck, a4, a5) = board in
  let cost = cost_of_build build
  in
    match has_enough_resources p cost with
    | true ->
        (match (List.length (reveal deck)) > 0 with
         | true ->
             let newinv = subtract_resources inv cost in
             let p = (c, (newinv, hand), (ks, lr, la)) in
             let (card, newdeck) = pick_one (reveal deck) in
             let cs = card :: (reveal turn.cardsbought) in
             let newturn =
               {
                 active = turn.active;
                 dicerolled = turn.dicerolled;
                 cardplayed = turn.cardplayed;
                 cardsbought = wrap_reveal cs;
                 tradesmade = turn.tradesmade;
                 pendingtrade = turn.pendingtrade;
               } in
             let hidden = wrap_reveal newdeck in
             let b = (a1, (insecs, roads), hidden, a4, a5)
             in (b, (p :: l), newturn, (colour, ActionRequest))
         | false -> HandleEndTurn.handle s)
    | false -> HandleEndTurn.handle s
  
let handle : state -> build -> state =
  fun (((board, plist, turn, (colour, request)) as s)) build ->
    match turn.active <> colour with
    | false -> HandleEndTurn.handle s
    | true ->
        (match build with
         | BuildRoad (c1, (p1, p2)) -> handle_road s build (c1, (p1, p2))
         | BuildTown point -> handle_town s build point
         | BuildCity point -> handle_city s build point
         | BuildCard -> handle_card s build)
  
