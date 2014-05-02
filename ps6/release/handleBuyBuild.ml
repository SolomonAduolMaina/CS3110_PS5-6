open Definition
  
open Constant
  
open Util
  
open Print
  
open MyUtil
  
open Util2
  
let handle_road (((board, plist, turn, _) as s)) build road =
  let (p, l) = get_player turn.active plist in
  let (c, (inv, hand), (ks, lr, la)) = p in
  let (a1, (insecs, roads), deck, a4, a5) = board in
  let valid = valid_road_build road roads in
  let cost = cost_of_build build in
  let enough = has_enough_resources p cost in
  let allowed = (player_roads_built c roads) < cMAX_ROADS_PER_PLAYER
  in
    match (valid, enough, allowed) with
    | (true, true, true) ->
        let newinv = subtract_resources inv cost in
        let p = (c, (newinv, hand), (ks, lr, la)) in
        let newroads = road :: roads in
        let b = (a1, (insecs, newroads), deck, a4, a5)
        in ((b, (p :: l), turn, ((turn.active), ActionRequest)), true)
    | _ -> ((HandleEndTurn.handle s), false)
  
let handle_town (((board, plist, turn, _) as s)) build point =
  let (p, l) = get_player turn.active plist in
  let (c, (inv, hand), ts) = p in
  let (a1, (insecs, roads), deck, a4, a5) = board in
  let not_built = is_none (List.nth insecs point) in
  let is_valid = valid_town_build c point (insecs, roads) in
  let cost = cost_of_build build in
  let enough = has_enough_resources p cost in
  let towns_built = player_settlements_built c Town insecs in
  let allowed = towns_built < cMAX_TOWNS_PER_PLAYER
  in
    match (not_built, is_valid, enough, allowed) with
    | (true, true, true, true) ->
        let newinv = subtract_resources inv cost in
        let p = (c, (newinv, hand), ts) in
        let newins = add_settlement point c Town insecs in
        let b = (a1, (newins, roads), deck, a4, a5)
        in ((b, (p :: l), turn, ((turn.active), ActionRequest)), true)
    | _ -> ((HandleEndTurn.handle s), false)
  
let handle_city (((board, plist, t, _) as s)) build point =
  let (p, l) = get_player t.active plist in
  let (c, (inv, hand), ts) = p in
  let (a1, (insecs, roads), deck, a4, a5) = board in
  let settlement = List.nth insecs point in
  let built = not (is_none settlement) in
  let (c', settlement) = get_some settlement in (* Danger *)
  let is_valid = (c = c') && (settlement = Town) in
  let cost = cost_of_build build in
  let enough = has_enough_resources p cost in
  let cities_built = player_settlements_built c City insecs in
  let allowed = cities_built < cMAX_CITIES_PER_PLAYER
  in
    match (built, is_valid, enough, allowed) with
    | (true, true, true, true) ->
        let newinv = subtract_resources inv cost in
        let p = (c, (newinv, hand), ts) in
        let newins = add_settlement point c City insecs in
        let b = (a1, (newins, roads), deck, a4, a5)
        in ((b, (p :: l), t, ((t.active), ActionRequest)), true)
    | _ -> ((HandleEndTurn.handle s), false)
  
let handle_card (((board, plist, turn, _) as s)) build =
  let (p, l) = get_player turn.active plist in
  let (c, (inv, hand), ts) = p in
  let (a1, (insecs, roads), deck, a4, a5) = board in
  let cost = cost_of_build build
  in
    match ((has_enough_resources p cost), ((List.length (reveal deck)) > 0))
    with
    | (true, true) ->
        let newinv = subtract_resources inv cost in
        let p = (c, (newinv, hand), ts) in
        let (card, newdeck) = pick_one (reveal deck) in
        let all_bought = card :: (reveal turn.cardsbought) in
        let newturn =
          {
            active = turn.active;
            dicerolled = turn.dicerolled;
            cardplayed = turn.cardplayed;
            cardsbought = wrap_reveal all_bought;
            tradesmade = turn.tradesmade;
            pendingtrade = turn.pendingtrade;
          } in
        let hidden = wrap_reveal newdeck in
        let b = (a1, (insecs, roads), hidden, a4, a5)
        in ((b, (p :: l), newturn, ((turn.active), ActionRequest)), true)
    | _ -> ((HandleEndTurn.handle s), false)
  
let handle : state -> build -> (state * bool) =
  fun (((_, _, turn, (colour, _)) as s)) build ->
    match (turn.active = colour) && (not (is_none turn.dicerolled)) with
    | false -> ((HandleEndTurn.handle s), false)
    | true ->
        (match build with
         | BuildRoad road -> handle_road s build road
         | BuildTown point -> handle_town s build point
         | BuildCity point -> handle_city s build point
         | BuildCard -> handle_card s build)
  
