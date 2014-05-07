open Definition
  
open Registry
  
open Constant
  
open Util
  
open BotUtil
    
let best_route points insecs roads = failwith ""
  
let index : 'a list -> (int * 'a) list =
  fun list ->
    let f (n, list) x = ((n + 1), ((n, x) :: list))
    in List.rev (snd (List.fold_left f (0, []) list))
  
let rec occupations colour indexed acc =
  match indexed with
  | [] -> acc
  | x :: xs ->
      (match x with
       | (_, None) -> occupations colour xs acc
       | (n, Some (c, _)) ->
           if c = colour then n :: acc else occupations colour xs acc)
  
let deck_size : cards -> int =
  fun hand ->
    match hand with | Hidden n -> n | Reveal list -> List.length list
  
let rec unweight : ('a * int) list -> 'a list =
  fun list -> match list with | [] -> [] | (a, _) :: xs -> a :: (unweight xs)
  
let leaders : player list -> intersection list -> player list =
  fun plist insecs ->
    let f list (((c, (inv, hand), (ks, lr, la)) as p)) =
      let cities = player_settlements_built c City insecs in
      let towns = player_settlements_built c Town insecs in
      let lr = if lr then cVP_LONGEST_ROAD else 0 in
      let la = if la then cVP_LARGEST_ARMY else 0
      in (p, (((cities + towns) + lr) + la)) :: list in
    let no_cards = List.fold_left f [] plist in
    let f (_, p1) (_, p2) = - (p1 - p2) in
    let sorted = List.sort f no_cards
    in
      match sorted with
      | (p1, ps1) :: (p2, ps2) :: xs ->
          if ps1 > ps2
          then unweight sorted
          else
            (let ((_, (_, hand1), _), (_, (_, hand2), _)) = (p1, p2)
             in
               if (deck_size hand1) >= (deck_size hand2)
               then unweight sorted
               else unweight ((p2, ps2) :: (p1, ps1) :: xs))
      | _ -> failwith "I'll be damned"
  
let stealable : color -> intersection list -> piece list =
  fun colour insecs ->
    let owned = occupations colour (index insecs) [] in
    let rec not_owned piece pieces =
      match piece <= cMAX_PIECE_NUM with
      | true ->
          let adjacent = piece_corners piece
          in
            if List.for_all (fun x -> not (List.mem x owned)) adjacent
            then not_owned (piece + 1) (piece :: pieces)
            else not_owned (piece + 1) pieces
      | false -> pieces
    in not_owned cMIN_PIECE_NUM []
  
let intersection : 'a list -> 'a list -> 'a list =
  fun l1 l2 -> List.filter (fun x -> List.mem x l2) l1
  
let play_knight : state -> playcard option =
  fun (board, plist, turn, (_, _)) ->
    let (_, rest) = get_player turn.active plist in
    let (_, (insecs, _), _, _, _) = board in
    let leaders = leaders rest insecs in
    let stealable = stealable turn.active insecs in
    let f opt (c, _, _) =
      let owned = occupations c (index insecs) [] in
      let intersection = intersection owned stealable in
      let steal = intersection <> []
      in
        match (is_none opt) && steal with
        | true -> Some (PlayKnight ((fst (pick_one intersection)), (Some c)))
        | false -> opt in
    let playcard = List.fold_left f None leaders
    in if is_none playcard then None else playcard
  
let account : cost -> cost -> (((resource * int) list) * (resource list)) =
  fun cost1 cost2 ->
    let f (n, l1, l2) v1 v2 =
      match n with
      | 0 ->
          if v2 = 0
          then ((n + 1), ((Brick, v1) :: l1), l2)
          else ((n + 1), l1, (Brick :: l2))
      | 1 ->
          if v2 = 0
          then ((n + 1), ((Wool, v1) :: l1), l2)
          else ((n + 1), l1, (Wool :: l2))
      | 2 ->
          if v2 = 0
          then ((n + 1), ((Ore, v1) :: l1), l2)
          else ((n + 1), l1, (Ore :: l2))
      | 3 ->
          if v2 = 0
          then ((n + 1), ((Grain, v1) :: l1), l2)
          else ((n + 1), l1, (Grain :: l2))
      | _ ->
          if v2 = 0
          then ((n + 1), ((Lumber, v1) :: l1), l2)
          else ((n + 1), l1, (Lumber :: l2)) in
    let (_, have, want) = cost_fold2 f (0, [], []) cost1 cost2
    in (have, want)
  
let play_plenty : state -> int -> playcard option =
  fun (board, plist, turn, (colour, _)) stage ->
    let ((c, (inv, hand), (ks, lr, la)), rest) = get_player colour plist in
    let (have, want) = account inv (stage_cost stage) in
    let (r1, l) = pick_one want
    in
      match l = [] with
      | true -> None
      | false ->
          let (r2, _) = pick_one l in Some (PlayYearOfPlenty (r1, (Some r2)))
  
let build_road state = failwith ""
  
let most_gain : player list -> resource list -> resource option =
  fun plist resources ->
    let f (resource, total) (c, (inv, hand), (ks, lr, la)) =
      let more = num_resource_in_inventory inv resource
      in (resource, (total + more)) in
    let g list resource =
      let p = List.fold_left f (resource, 0) plist in p :: list in
    let pairs = List.fold_left g [] resources in
    let f (_, w1) (_, w2) = - (w1 - w2) in
    let sorted = List.sort f pairs
    in
      match sorted with
      | [] -> failwith "No way"
      | (resource, total) :: _ -> if total > 0 then Some resource else None
  
let play_monopoly (board, plist, t, (colour, _)) stage =
  let ((c, (inv, hand), (ks, lr, la)), rest) = get_player t.active plist in
  let (have, want) = account inv (stage_cost stage) in
  let resource = most_gain plist want
  in
    match is_none resource with
    | true -> None
    | false -> Some (PlayMonopoly (get_some resource))
  
let what_card : state -> card -> int -> playcard option =
  fun state card stage ->
    match card with
    | Knight -> play_knight state
    | RoadBuilding ->
        let (r1, s) = build_road state in
        let (r2, _) = build_road s
        in
          if is_none r2
          then None
          else Some (PlayRoadBuilding (r1, (Some (get_some r2))))
    | YearOfPlenty -> play_plenty state stage
    | Monopoly -> play_monopoly state stage
    | _ -> failwith "No way"
  
let rec play_card : state -> card list -> int -> playcard option =
  fun state hand stage ->
    match hand with
    | [] -> None
    | card :: cards ->
        let playcards = [ Knight; RoadBuilding; YearOfPlenty; Monopoly ]
        in
          (match List.mem card playcards with
           | true ->
               let card = what_card state card stage
               in
                 (match is_none card with
                  | false -> card
                  | true -> play_card state cards stage)
           | false -> play_card state cards stage)
  
let rec trade : state -> int -> bool -> move =
  fun (((board, plist, turn, (colour, _)) as s)) stage tried ->
    let ((c, (inv, hand), (ks, lr, la)), rest) = get_player colour plist in
    let cost = stage_cost stage in
    let (have, want) = account inv cost in
    let f (resource, plist) (c, (inv, _), _) =
      if enough_resources inv (single_resource_cost resource)
      then (resource, ((c, resource) :: plist))
      else (resource, plist) in
    let g plist resource =
      let inters = List.fold_left f (resource, []) rest
      in (snd inters) @ plist in
    let possible = List.fold_left g [] want in
    let f (_, h1) (_, h2) = - (h1 - h2) in
    let sorted = List.sort f have
    in
      match (sorted, possible) with
      | ([], _) -> handle s stage tried
      | (_, []) -> handle s stage tried
      | _ ->
          let (res, _) = List.hd sorted in
          let ((c, resource), _) = pick_one possible in
          let give = single_resource_cost res in
          let gain = single_resource_cost resource
          in Action (DomesticTrade (c, give, gain))

and domestic_trade : state -> int -> move =
  fun (((board, plist, turn, (colour, _)) as s)) stage ->
    let ((c, (inv, hand), (ks, lr, la)), l) = get_player colour plist in
    let cost = stage_cost stage in
    let (have, want) = account inv cost in
    let ((_, ports), (insecs, roads), _, _, _) = board in
    let f (resource, _) = least_ratio colour ports insecs resource in
    let least_ratios = List.map f have in
    let assoc = List.combine have least_ratios in
    let f (_, r1) (_, r2) = r1 - r2 in
    let sorted = List.sort f assoc in
    let f opt ((res, have), ratio) =
      if (is_none opt) && (have >= ratio) then Some (res, ratio) else opt in
    let give = List.fold_left f None sorted
    in
      match give with
      | None -> handle s stage true
      | Some (res, _) ->
          let (gain, _) = pick_one want in Action (MaritimeTrade (res, gain))

and handle : state -> int -> bool -> move =
  fun (((board, plist, turn, (colour, _)) as s)) stage tried ->
    let ((c, (inv, hand), (ks, lr, la)), l) = get_player colour plist in
    let card = play_card s (reveal hand) stage
    in
      match ((turn.cardplayed), (is_none card)) with
      | (false, false) -> Action (PlayCard (get_some card))
      | _ ->
          let enough = enough_resources inv (stage_cost stage) in
          let limit = cNUM_TRADES_PER_TURN
          in
            (match (enough, (turn.tradesmade)) with
             | (true, _) -> build s stage tried
             | (false, n) when n < (limit / 2) ->
                 if not tried
                 then domestic_trade s stage
                 else trade s (stage + 1) tried
             | (false, n) when (n >= (limit / 2)) && (n < limit) ->
                 trade s (stage + 1) tried
             | _ ->
                 if not tried then domestic_trade s stage else Action EndTurn)

and build_city (board, plist, turn, (_, _)) =
  let (_, (insecs, _), _, _, _) = board in
  let occupations = occupations turn.active (index insecs) [] in
  let f n = (snd (get_some (List.nth insecs n))) = Town
  in Action (BuyBuild (BuildCity (List.find f occupations)))

and build_road (board, plist, turn, (_, _)) =
  let (_, (insecs, roads), _, _, _) = board in
  let mine = get_player_roads turn.active roads in
  let player_paths = player_paths turn.active mine insecs in
  let f l = [ fst (List.hd l); snd (List.hd (List.rev l)) ] in
  let points = List.concat (List.map f player_paths)
  in best_route points insecs roads

and build : state -> int -> bool -> move =
  fun (((board, plist, turn, (_, _)) as s)) stage tried ->
    match stage_cost stage with
    | n when n = cCOST_TOWN -> build_town s stage tried
    | n when n = cCOST_CITY -> build_city s
    | n when n = cCOST_ROAD -> build_road s
    | _ -> Action (BuyBuild BuildCard)

and build_town (((board, plist, turn, (_, _)) as s)) stage tried =
  let (_, (insecs, roads), _, _, _) = board in
  let f (c1, _) = turn.active = c1 in
  let mine = List.filter f roads
  in
    let module Aset =
      Set.Make(struct let compare = Pervasives.compare

                         type t = point
                          end)
    in
      let f set (_, (p1, p2)) = let s1 = Aset.add p1 set in Aset.add p2 s1 in
      let set = List.fold_left f Aset.empty mine in
      let potentials = Aset.elements set in
      let f x = valid_town_build turn.active x (insecs, roads) in
      let safes = List.filter f potentials
      in
        match safes with
        | [] -> handle s stage tried
        | _ -> let (p, _) = pick_one safes in Action (BuyBuild (BuildTown p))
  
