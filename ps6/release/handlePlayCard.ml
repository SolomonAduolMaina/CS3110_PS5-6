open Definition
  
open Constant
  
open Util
  
open Print
  
open MyUtil
  
let remove_card : card -> player -> player =
  fun card_type p ->
    let (colour, (inv, hand), ts) = p in
    let p card = card = card_type in
    let newdeck = list_memremove p (reveal hand)
    in (colour, (inv, (wrap_reveal newdeck)), ts)
  
let played_card : turn -> turn =
  fun t ->
    {
      active = t.active;
      dicerolled = t.dicerolled;
      cardplayed = true;
      cardsbought = t.cardsbought;
      tradesmade = t.tradesmade;
      pendingtrade = t.pendingtrade;
    }
  
let update_armies : color -> player list -> player list =
  fun c plist ->
    let ((c, bank, (ks, lr, la)), l) = get_player c plist in
    let player = (c, bank, ((ks + 1), lr, la)) in
    let f ((c, bank, (ks, lr, la)), plist) (c', bank', (ks', lr', la')) =
      match ks' > ks with
      | true ->
          let nl = (c, bank, (ks, lr, false)) :: plist
          in ((c', bank', (ks', lr', la')), nl)
      | false ->
          let nl = (c', bank', (ks', lr', false)) :: plist
          in ((c, bank, (ks, lr, la)), nl) in
    let ((c, bank, (ks, lr, la)), nl) = List.fold_left f (player, []) l
    in
      if ks >= cMIN_LARGEST_ARMY
      then (c, bank, (ks, lr, true)) :: nl
      else (c, bank, (ks, lr, false)) :: nl
  
let handle_knight : state -> robbermove -> (state * bool) =
  fun (board, plist, t, (c, r)) robbermove ->
    let (p, l) = get_player c plist in
    let nl = (remove_card Knight p) :: l in
    let nl = update_armies c nl in
    let (piece, opt) = robbermove in
    let (map, structs, deck, discard, _) = board in
    let board = (map, structs, deck, discard, piece)
    in
      match not (is_none opt) with
      | true ->
          let victim = get_some opt in
          let (_, (insecs, _), _, _, _) = board in
          let touches = has_settlement_around_piece piece victim insecs in
          let nl =
            (match touches with
             | true -> steal_from_and_give_to victim c nl
             | false -> nl)
          in ((board, nl, (played_card t), (c, ActionRequest)), true)
      | false -> ((board, nl, (played_card t), (c, ActionRequest)), false)
  
let handle_road : state -> (road * (road option)) -> (state * bool) =
  fun (((board, plist, t, (c, r)) as s)) (road, opt) ->
    let handle_road_helper : state -> road -> (state * bool) =
      fun (board, plist, t, (c, r)) road ->
        let (p, l) = get_player c plist in
        let nl = (remove_card RoadBuilding p) :: l in
        let (c, (inv, hand), (ks, lr, la)) = p in
        let (a1, (insecs, roads), deck, a4, a5) = board in
        let valid = valid_road_build road roads in
        let allowed = (player_roads_built c roads) < cMAX_ROADS_PER_PLAYER
        in
          match (valid, allowed) with
          | (true, true) ->
              let newroads = road :: roads in
              let b = (a1, (insecs, newroads), deck, a4, a5)
              in ((b, nl, t, (c, ActionRequest)), true)
          | _ -> ((board, nl, t, (c, r)), false)
    in
      match handle_road_helper s road with
      | (first, true) ->
          (match is_none opt with
           | true -> (first, true)
           | false ->
               (match handle_road_helper first (get_some opt) with
                | (both, true) -> (both, true)
                | _ -> ((HandleEndTurn.handle first), false)))
      | (failed, false) ->
          (match is_none opt with
           | true -> ((HandleEndTurn.handle failed), false)
           | false ->
               (match handle_road_helper s (get_some opt) with
                | (second, true) -> ((HandleEndTurn.handle second), false)
                | _ -> ((HandleEndTurn.handle failed), false)))
  
let handle_plenty :
  state -> (resource * (resource option)) -> (state * bool) =
  fun (board, plist, t, (c, r)) (resource, opt) ->
    let (p, l) = get_player c plist in
    let (c, (inv, hand), (ks, lr, la)) = remove_card YearOfPlenty p in
    let newinv = plus_resources inv (single_resource_cost resource)
    in
      match is_none opt with
      | true ->
          let p = (c, (newinv, hand), (ks, lr, la))
          in ((board, (p :: l), t, (c, ActionRequest)), true)
      | false ->
          let more = single_resource_cost (get_some opt) in
          let newinv = plus_resources newinv more in
          let p = (c, (newinv, hand), (ks, lr, la))
          in ((board, (p :: l), t, (c, ActionRequest)), true)
  
let handle_monopoly : state -> resource -> (state * bool) =
  fun (board, plist, t, _) resource ->
    let (p, l) = get_player t.active plist in
    let p = remove_card Monopoly p in
    let f (n, list) (c, ((b, w, o, l, g), hand), q) =
      let n' = num_resource_in_inventory (b, w, o, l, g) resource
      in
        match resource with
        | Brick -> ((n + n'), ((c, ((0, w, o, l, g), hand), q) :: list))
        | Wool -> ((n + n'), ((c, ((b, 0, o, l, g), hand), q) :: list))
        | Ore -> ((n + n'), ((c, ((b, w, 0, l, g), hand), q) :: list))
        | Grain -> ((n + n'), ((c, ((b, w, o, 0, g), hand), q) :: list))
        | Lumber -> ((n + n'), ((c, ((b, w, o, l, 0), hand), q) :: list)) in
    let (n, victims) = List.fold_left f (0, []) l in
    let plist = p :: victims in
    let plist =
      match resource with
      | Brick -> add_resources_to_player t.active (n, 0, 0, 0, 0) plist
      | Wool -> add_resources_to_player t.active (0, n, 0, 0, 0) plist
      | Ore -> add_resources_to_player t.active (0, 0, n, 0, 0) plist
      | Grain -> add_resources_to_player t.active (0, 0, 0, n, 0) plist
      | Lumber -> add_resources_to_player t.active (0, 0, 0, 0, n) plist
    in ((board, plist, t, ((t.active), ActionRequest)), true)
  
let handle : state -> playcard -> (state * bool) =
  fun (((board, plist, t, (c, r)) as s)) playcard ->
    match t.active = c with
    | true ->
        let (map, structs, deck, discard, robber) = board in
        let discard = (card_of_playcard playcard) :: discard in
        let board = (map, structs, deck, discard, robber) in
        let s = (board, plist, t, (c, r))
        in
          (match playcard with
           | PlayKnight robbermove -> handle_knight s robbermove
           | PlayRoadBuilding (road, opt) -> handle_road s (road, opt)
           | PlayYearOfPlenty (res, opt) -> handle_plenty s (res, opt)
           | PlayMonopoly resource -> handle_monopoly s resource)
    | false -> ((HandleEndTurn.handle s), false)
  
