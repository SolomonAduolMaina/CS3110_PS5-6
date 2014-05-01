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
  
let update_turn : turn -> turn =
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
          let newlist = (c, bank, (ks, lr, false)) :: plist
          in ((c', bank', (ks', lr', la')), newlist)
      | false ->
          let newlist = (c', bank', (ks', lr', false)) :: plist
          in ((c, bank, (ks, lr, la)), newlist) in
    let ((c, bank, (ks, lr, la)), newlist) = List.fold_left f (player, []) l
    in
      if ks >= cMIN_LARGEST_ARMY
      then (c, bank, (ks, lr, true)) :: newlist
      else (c, bank, (ks, lr, false)) :: newlist
  
let handle_knight : state -> robbermove -> state =
  fun (board, plist, t, (c, r)) robbermove ->
    let (p, l) = get_player c plist in
    let newlist = (remove_card Knight p) :: l in (* Could fail *)
    let newlist = update_armies c newlist in
    let (piece, opt) = robbermove in
    let victim = get_some opt in (* Could fail *)
    let (_, (inter_list, _), _, _, _) = board in
    let is_valid = has_settlement_around_piece piece victim inter_list in
    let newlist =
      match is_valid with
      | true -> steal_from_and_give_to victim c newlist
      | (* Could fail *) false -> newlist
    in (board, newlist, (update_turn t), (c, ActionRequest))
  
let handle_road : state -> (road * (road option)) -> state =
  fun (((board, plist, t, (c, r)) as s)) ((c1, (p1, p2)), opt) ->
    let handle_helper : state -> road -> (state * bool) =
      fun (board, plist, t, (c, r)) (c1, (p1, p2)) ->
        let (p, l) = get_player c plist in
        let newlist = (remove_card RoadBuilding p) :: l in (* Could fail *)
        let (c, (inv, hand), (ks, lr, la)) = p in
        let (a1, (insecs, roads), deck, a4, a5) = board in
        let not_bought = not (road_bought (p1, p2) roads) in
        let is_valid = (is_valid_line (p1, p2)) && (c1 = c) in
        let allowed = (player_roads_built c roads) < cMAX_ROADS_PER_PLAYER
        in
          match (not_bought, allowed, is_valid) with
          | (true, true, true) ->
              let newroads = (c1, (p1, p2)) :: roads in
              let b = (a1, (insecs, newroads), deck, a4, a5)
              in ((b, newlist, t, (c, ActionRequest)), true)
          | _ -> ((board, newlist, t, (c, r)), false)
    in
      match handle_helper s (c1, (p1, p2)) with
      | (first, true) ->
          (match is_none opt with
           | true -> first
           | false ->
               (match handle_helper first (get_some opt) with
                | (both, true) -> both
                | _ -> HandleEndTurn.handle first))
      | (failed, false) ->
          (match is_none opt with
           | true -> HandleEndTurn.handle failed
           | false ->
               (match handle_helper s (get_some opt) with
                | (second, true) -> HandleEndTurn.handle second
                | _ -> HandleEndTurn.handle failed))
  
let handle_plenty : state -> (resource * (resource option)) -> state =
  fun (board, plist, t, (c, r)) (resource, opt) ->
    let (p, l) = get_player c plist in
    let (c, (inv, hand), (ks, lr, la)) = remove_card YearOfPlenty p in
    let newinv = plus_resources inv (single_resource_cost resource)
    in
      match is_none opt with
      | true ->
          let p = (c, (newinv, hand), (ks, lr, la))
          in (board, (p :: l), t, (c, ActionRequest))
      | false ->
          let more = single_resource_cost (get_some opt) in
          let newinv = plus_resources newinv more in
          let p = (c, (newinv, hand), (ks, lr, la))
          in (board, (p :: l), t, (c, ActionRequest))
  
let handle_monopoly : state -> resource -> state =
  fun (board, plist, t, (c, r)) resource ->
    let (p, l) = get_player c plist in
    let (((c, (inv, hand), (ks, lr, la)) as p)) = remove_card Monopoly p in
    let f (n, list) (c, ((b, w, o, l, g), hand), q) =
      let n' = num_resource_in_inventory inv resource
      in
        match resource with
        | Brick -> ((n + n'), ((c, ((0, w, o, l, g), hand), q) :: list))
        | Wool -> ((n + n'), ((c, ((b, 0, o, l, g), hand), q) :: list))
        | Ore -> ((n + n'), ((c, ((b, w, 0, l, g), hand), q) :: list))
        | Grain -> ((n + n'), ((c, ((b, w, o, 0, g), hand), q) :: list))
        | Lumber -> ((n + n'), ((c, ((b, w, o, l, 0), hand), q) :: list)) in
    let (n, plist) = List.fold_left f (0, []) l in
    let newlist = p :: plist in
    let updated =
      match resource with
      | Brick -> add_resources_to_player c (n, 0, 0, 0, 0) newlist
      | Wool -> add_resources_to_player c (0, n, 0, 0, 0) newlist
      | Ore -> add_resources_to_player c (0, 0, n, 0, 0) newlist
      | Grain -> add_resources_to_player c (0, 0, 0, n, 0) newlist
      | Lumber -> add_resources_to_player c (0, 0, 0, 0, n) newlist
    in (board, updated, t, (c, ActionRequest))
  
let handle : state -> playcard -> state =
  fun (((board, plist, t, (c, r)) as s)) playcard ->
    match playcard with
    | PlayKnight robbermove -> handle_knight s robbermove
    | PlayRoadBuilding (road, opt) -> handle_road s (road, opt)
    | PlayYearOfPlenty (resource, opt) -> handle_plenty s (resource, opt)
    | PlayMonopoly resource -> handle_monopoly s resource
  
