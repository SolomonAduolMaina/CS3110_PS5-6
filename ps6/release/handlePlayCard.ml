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
      match (ks' >= ks) && la' with
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
  fun (((board, plist, t, (c, r)) as s)) (piece, opt) ->
    let plist = update_armies c plist in
    let (map, structs, deck, discard, _) = board in
    let board = (map, structs, deck, discard, piece)
    in
      match not (is_none opt) with
      | true ->
          let victim = get_some opt in
          let sensible = victim <> t.active in
          let (_, (insecs, _), _, _, _) = board in
          let touches = has_settlement_around_piece piece victim insecs
          in
            (match sensible && touches with
             | true ->
                 let plist = steal_from_and_give_to victim t.active plist
                 in ((board, plist, t, (c, ActionRequest)), true)
             | false -> ((HandleEndTurn.handle s), false))
      | false -> ((HandleEndTurn.handle s), false)
  
let handle_road : state -> (road * (road option)) -> (state * bool) =
  fun (((board, plist, t, (c, r)) as s)) (road, opt) ->
    let handle_road_helper : state -> road -> (state * bool) =
      fun (board, plist, t, (c, r)) road ->
        let (a1, (insecs, roads), deck, a4, a5) = board in
        let valid = valid_road_build road roads in
        let allowed = (player_roads_built c roads) < cMAX_ROADS_PER_PLAYER
        in
          match (valid, allowed) with
          | (true, true) ->
              let roads = road :: roads in
              let plist = update_longest_road_trophy plist roads insecs in
              let b = (a1, (insecs, roads), deck, a4, a5)
              in ((b, plist, t, (c, ActionRequest)), true)
          | _ -> ((HandleEndTurn.handle s), false)
    in
      match handle_road_helper s road with
      | (first, true) ->
          (match is_none opt with
           | true -> (first, true)
           | false ->
               (match handle_road_helper first (get_some opt) with
                | (both, true) -> (both, true)
                | _ -> ((HandleEndTurn.handle s), false)))
      | (failed, false) -> ((HandleEndTurn.handle s), false)
  
let handle_plenty (board, plist, t, (c, r)) (resource, opt) =
  let ((c, (inv, hand), ts), rest) = get_player c plist in
  let newinv = plus_resources inv (single_resource_cost resource)
  in
    match is_none opt with
    | true ->
        let p = (c, (newinv, hand), ts)
        in ((board, (p :: rest), t, (c, ActionRequest)), true)
    | false ->
        let more = single_resource_cost (get_some opt) in
        let newinv = plus_resources newinv more in
        let p = (c, (newinv, hand), ts)
        in ((board, (p :: rest), t, (c, ActionRequest)), true)
  
let handle_monopoly : state -> resource -> (state * bool) =
  fun (board, plist, t, _) resource ->
    let (p, rest) = get_player t.active plist in
    let f (n, list) (c, ((b, w, o, l, g), hand), ts) =
      let n' = num_resource_in_inventory (b, w, o, l, g) resource
      in
        match resource with
        | Brick -> ((n + n'), ((c, ((0, w, o, l, g), hand), ts) :: list))
        | Wool -> ((n + n'), ((c, ((b, 0, o, l, g), hand), ts) :: list))
        | Ore -> ((n + n'), ((c, ((b, w, 0, l, g), hand), ts) :: list))
        | Grain -> ((n + n'), ((c, ((b, w, o, 0, g), hand), ts) :: list))
        | Lumber -> ((n + n'), ((c, ((b, w, o, l, 0), hand), ts) :: list)) in
    let (n, victims) = List.fold_left f (0, []) rest in
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
    match (t.active = c) && (not t.cardplayed) with
    | true ->
        let card = card_of_playcard playcard in
        let ((c, (inv, hand), ts), rest) = get_player t.active plist
        in
          (match List.mem card (reveal hand) with
           | true ->
               let (map, structs, deck, discard, robber) = board in
               let discard = card :: discard in
               let board = (map, structs, deck, discard, robber) in
               let p = remove_card card (c, (inv, hand), ts) in
               let q = (board, (p :: rest), (played_card t), (c, r))
               in
                 (match playcard with
                  | PlayKnight robbermove ->
                      (match handle_knight q robbermove with
                       | (_, false) -> ((HandleEndTurn.handle s), false)
                       | n -> n)
                  | PlayRoadBuilding (road, opt) ->
                      (match handle_road q (road, opt) with
                       | (_, false) -> ((HandleEndTurn.handle s), false)
                       | n -> n)
                  | PlayYearOfPlenty (res, opt) -> handle_plenty q (res, opt)
                  | PlayMonopoly resource -> handle_monopoly q resource)
           | false -> ((HandleEndTurn.handle s), false))
    | false -> ((HandleEndTurn.handle s), false)
  
