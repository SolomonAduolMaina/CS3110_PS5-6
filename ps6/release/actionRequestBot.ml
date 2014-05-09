open Definition
  
open Registry
  
open Constant
  
open Util
  
open Print
  
open BotUtil
  
let index : 'a list -> (int * 'a) list =
  fun list ->
    let f (n, list) x = ((n + 1), ((n, x) :: list))
    in List.rev (snd (List.fold_left f (0, []) list))
  
let rec occupations colour indexed acc separate =
  match indexed with
  | [] -> acc
  | x :: xs ->
      (match x with
       | (_, None) -> occupations colour xs acc separate
       | (n, Some (c, setl)) ->
           (match separate with
            | false ->
                if c = colour
                then n :: acc
                else occupations colour xs acc separate
            | true ->
                if (c = colour) && (setl = Town)
                then n :: acc
                else occupations colour xs acc separate))
  
let build_city (board, plist, turn, (_, _)) =
  let (_, (insecs, _), _, _, _) = board in
  let occupations = occupations turn.active (index insecs) [] true in
  let f n = (snd (get_some (List.nth insecs n))) = Town
  in Action (BuyBuild (BuildCity (List.find f occupations)))
  
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
    let owned = occupations colour (index insecs) [] false in
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
      let owned = occupations c (index insecs) [] false in
      let intersection = intersection owned stealable in
      let steal = intersection <> []
      in
        match (is_none opt) && steal with
        | true ->
            let point = fst (pick_one intersection) in
            let piece = fst (pick_one (adjacent_pieces point))
            in Some (PlayKnight (piece, (Some c)))
        | false -> opt in
    let playcard = List.fold_left f None leaders
    in if is_none playcard then None else playcard
  
let account : cost -> cost -> (((resource * int) list) * (resource list)) =
  fun cost1 cost2 ->
    let f (n, l1, l2) v1 v2 =
      match n with
      | 0 ->
          (match v2 = 0 with
           | true ->
               if v1 > 0
               then ((n + 1), ((Brick, v1) :: l1), l2)
               else ((n + 1), l1, l2)
           | false ->
               if v1 > v2
               then ((n + 1), ((Brick, v1) :: l1), l2)
               else
                 if v1 < v2
                 then ((n + 1), l1, (Brick :: l2))
                 else ((n + 1), l1, l2))
      | 1 ->
          (match v2 = 0 with
           | true ->
               if v1 > 0
               then ((n + 1), ((Wool, v1) :: l1), l2)
               else ((n + 1), l1, l2)
           | false ->
               if v1 > v2
               then ((n + 1), ((Wool, v1) :: l1), l2)
               else
                 if v1 < v2
                 then ((n + 1), l1, (Wool :: l2))
                 else ((n + 1), l1, l2))
      | 2 ->
          (match v2 = 0 with
           | true ->
               if v1 > 0
               then ((n + 1), ((Ore, v1) :: l1), l2)
               else ((n + 1), l1, l2)
           | false ->
               if v1 > v2
               then ((n + 1), ((Ore, v1) :: l1), l2)
               else
                 if v1 < v2
                 then ((n + 1), l1, (Ore :: l2))
                 else ((n + 1), l1, l2))
      | 3 ->
          (match v2 = 0 with
           | true ->
               if v1 > 0
               then ((n + 1), ((Grain, v1) :: l1), l2)
               else ((n + 1), l1, l2)
           | false ->
               if v1 > v2
               then ((n + 1), ((Grain, v1) :: l1), l2)
               else
                 if v1 < v2
                 then ((n + 1), l1, (Grain :: l2))
                 else ((n + 1), l1, l2))
      | _ ->
          (match v2 = 0 with
           | true ->
               if v1 > 0
               then ((n + 1), ((Lumber, v1) :: l1), l2)
               else ((n + 1), l1, l2)
           | false ->
               if v1 > v2
               then ((n + 1), ((Lumber, v1) :: l1), l2)
               else
                 if v1 < v2
                 then ((n + 1), l1, (Lumber :: l2))
                 else ((n + 1), l1, l2)) in
    let (_, have, want) = cost_fold2 f (0, [], []) cost1 cost2
    in (have, want)
  
let play_plenty : state -> int -> playcard option =
  fun (board, plist, turn, (colour, _)) stage ->
    let ((c, (inv, hand), (ks, lr, la)), rest) = get_player colour plist in
    let (have, want) = account inv (stage_cost stage)
    in
      match want = [] with
      | true -> None
      | false ->
          let (r1, l) = pick_one want
          in
            (match l = [] with
             | true -> None
             | false ->
                 let (r2, _) = pick_one l
                 in Some (PlayYearOfPlenty (r1, (Some r2))))
  
let most_gain : player list -> resource list -> resource option =
  fun plist resources ->
    let f (resource, total) (c, (inv, hand), (ks, lr, la)) =
      let more = num_resource_in_inventory inv resource
      in (resource, (total + more)) in
    let g list resource =
      let p = List.fold_left f (resource, 0) plist in p :: list in
    let pairs = List.fold_left g [] resources in
    let f (_, w1) (_, w2) = - (w1 - w2) in
    let sorted = List.sort f pairs in
    let (resource, total) = List.hd sorted
    in if total > 0 then Some resource else None
  
let play_monopoly (board, plist, t, (colour, _)) stage =
  let ((c, (inv, hand), (ks, lr, la)), rest) = get_player t.active plist in
  let (have, want) = account inv (stage_cost stage)
  in
    match want = [] with
    | true -> None
    | false ->
        let resource = most_gain plist want
        in
          (match is_none resource with
           | true -> None
           | false -> Some (PlayMonopoly (get_some resource)))
  
module PMap =
  Map.Make(struct let compare = Pervasives.compare

                     type t = point
                      end)
  
let not_enemy : color -> point -> intersection list -> bool =
  fun colour point insecs ->
    let setl = List.nth insecs point
    in (is_none setl) || ((fst (get_some setl)) = colour)
  
let rec shortest_path colour p2 insecs queue map =
  let p1 = Queue.pop queue
  in
    match p1 = p2 with
    | true -> List.rev (p1 :: (PMap.find p1 map))
    | false ->
        let adjacent = adjacent_points p1 in
        let f (queue, map) p =
          if not_enemy colour p insecs
          then
            (let in_queue =
               Queue.fold (fun b x -> b || (x = p)) false queue in
             let in_table = PMap.mem p map
             in
               match (in_queue, in_table) with
               | (true, true) -> (queue, map)
               | _ ->
                   let () = Queue.add p queue in
                   let l = PMap.find p1 map
                   in (queue, (PMap.add p (p1 :: l) map)))
          else (queue, map) in
        let (queue, map) = List.fold_left f (queue, map) adjacent
        in shortest_path colour p2 insecs queue map
  
let best_shortest colour (p1, p2) (p3, p4) insecs =
  let f (p1, p2) =
    let queue = Queue.create () in
    let map = PMap.empty in
    let () = Queue.add p1 queue in
    let map = PMap.add p1 [] map
    in List.length (shortest_path colour p2 insecs queue map) in
  let pairs = [ (p1, p3); (p1, p4); (p2, p3); (p2, p4) ] in
  let lengths = List.map f pairs in
  let assoc = List.combine pairs lengths in
  let f (_, l1) (_, l2) = l1 - l2 in
  let sorted = List.sort f assoc in
  let f (_, l1) (_, l2) = - (l1 - l2) in
  let reversed = List.sort f assoc
  in ((Some (fst (List.hd sorted))), (Some (fst (List.hd reversed))))
  
let road_points : road list -> point list =
  fun roads -> List.concat (List.map (fun (_, (p1, p2)) -> [ p1; p2 ]) roads)
  
let extend (p1, p2) colour insecs roads =
  let f (p1, opt) p2 =
    let can_build = valid_road_build (colour, (p1, p2)) roads insecs
    in
      if (is_none opt) && can_build
      then (p1, (Some (colour, (p1, p2))))
      else (p1, opt) in
  let g opt p1 =
    if is_none opt
    then snd (List.fold_left f (p1, None) (adjacent_points p1))
    else opt in
  let road = List.fold_left g None [ p1; p2 ]
  in
    match road with
    | None -> (None, None)
    | Some (c, (a, b)) ->
        let next = List.fold_left g None [ b ] in ((Some (c, (a, b))), next)
  
let rec continue points (((board, plist, t, next) as s)) origin =
  let (a1, (insecs, roads), a2, a3, a4) = board in
  let f (p1, opt) p2 =
    let valid = valid_road_build ((t.active), (p1, p2)) roads insecs
    in
      if (is_none opt) && valid
      then (p1, (Some (t.active, (p1, p2))))
      else (p1, opt) in
  let g opt p =
    if not (is_none opt)
    then opt
    else snd (List.fold_left f (p, None) (adjacent_points p))
  in
    match List.fold_left g None points with
    | None -> ((None, s, None), origin)
    | Some road ->
        let roads = road :: roads in
        let board = (a1, (insecs, roads), a2, a3, a4) in
        let s = (board, plist, t, next) in (((Some road), s, None), origin)
  
let rec which_road path points point s origin =
  match path with
  | [] -> (None, None)
  | x :: y :: xs ->
      (match ((List.mem x points), (List.mem y points)) with
       | (true, false) ->
           (match xs with
            | z :: _ -> ((Some (x, y)), (Some (y, point)))
            | _ -> ((Some (x, y)), None))
       | (true, true) ->
           (match xs with
            | [] | [ _ ] -> ((Some (x, y)), None)
            | _ -> which_road (y :: xs) points point s origin)
       | (false, false) ->
           let ((opt, _, _), _) = continue points s origin in
           let (_, road) = get_some opt in ((Some road), None)
       | (false, true) -> which_road (y :: xs) points point s origin)
  | [ x ] -> ((Some (x, point)), None)
  
let rec build_road colour (((board, plist, turn, next) as s)) opt origin =
  let (a1, (insecs, roads), a2, a3, a4) = board
  in
    match opt with
    | None ->
        let mine = get_player_roads turn.active roads
        in
          (match (List.length mine) = 2 with
           | true ->
               let (_, l1) = List.hd mine in
               let (_, l2) = List.hd (List.tl mine) in
               let (road, origin) = best_shortest colour l1 l2 insecs
               in build_road turn.active s road origin
           | false ->
               (match not (is_none origin) with
                | true ->
                    let (r, n) = extend (get_some origin) colour insecs roads
                    in
                      (match is_none r with
                       | true -> continue (road_points mine) s origin
                       | false ->
                           let roads = (get_some r) :: roads in
                           let board = (a1, (insecs, roads), a2, a3, a4) in
                           let s = (board, plist, turn, next)
                           in
                             if is_none n
                             then ((r, s, None), None)
                             else ((r, s, None), (Some (snd (get_some n)))))
                | false -> continue (road_points mine) s origin))
    | Some (p1, p2) ->
        let queue = Queue.create () in
        let map = PMap.empty in
        let () = Queue.add p1 queue in
        let map = PMap.add p1 [] map in
        let path = shortest_path turn.active p2 insecs queue map in
        let points = road_points (get_player_roads turn.active roads)
        in
          (match which_road path points p2 s origin with
           | (None, _) -> failwith "No way"
           | (Some (x, y), None) ->
               let roads = ((turn.active), (x, y)) :: roads in
               let board = (a1, (insecs, roads), a2, a3, a4) in
               let s = (board, plist, turn, next) in
               let road = Some (turn.active, (x, y))
               in ((road, s, None), origin)
           | (Some (x, y), q) ->
               let roads = ((turn.active), (x, y)) :: roads in
               let board = (a1, (insecs, roads), a2, a3, a4) in
               let s = (board, plist, turn, next) in
               let r = Some (turn.active, (x, y)) in ((r, s, q), origin))
  
let what_card (((_, _, t, _) as state)) card stage opt origin =
  match card with
  | Knight -> ((play_knight state), (opt, origin))
  | RoadBuilding ->
      let ((r1, s, opt1), _) = build_road t.active state opt origin in
      let ((r2, _, opt2), _) = build_road t.active s opt1 origin
      in
        (match ((is_none r1), (is_none r2)) with
         | (true, _) | (_, true) -> (None, (opt, origin))
         | _ ->
             let (r1, r2) = ((get_some r1), (get_some r2))
             in ((Some (PlayRoadBuilding (r1, (Some r2)))), (opt2, origin)))
  | YearOfPlenty -> ((play_plenty state stage), (opt, origin))
  | Monopoly -> ((play_monopoly state stage), (opt, origin))
  | _ -> failwith "No way"
  
let rec play_card state hand stage opt origin =
  match hand with
  | [] -> (None, (opt, origin))
  | card :: cards ->
      let playcards = [ Knight; RoadBuilding; YearOfPlenty; Monopoly ]
      in
        (match List.mem card playcards with
         | true ->
             let (card, (opt1, origin)) =
               what_card state card stage opt origin
             in
               (match is_none card with
                | false -> (card, (opt1, origin))
                | true -> play_card state cards stage opt origin)
         | false -> play_card state cards stage opt origin)
  
let rec doable inv cost board colour =
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
    | None -> (false, None)
    | Some (res, ratio) ->
        let loss = n_of_resource ratio res in
        let get = fst (pick_one want) in
        let gain = single_resource_cost get in
        let net = subtract_resources (plus_resources inv gain) loss
        in
          if enough_resources net cost
          then (true, (Some (res, get)))
          else doable net cost board colour
  
let rec maritime_trade (((b, plist, t, _) as s)) stage opt origin mar =
  let ((c, (inv, hand), (ks, lr, la)), l) = get_player t.active plist in
  let cost = stage_cost stage
  in
    match enough_resources inv cost with
    | true -> build s stage opt origin mar
    | false ->
        (match ((doable inv cost b t.active), mar) with
         | ((true, Some (a, b)), _) ->
             ((Action (MaritimeTrade (a, b))), (opt, origin))
         | ((false, _), true) -> build s stage opt origin mar
         | _ -> ((Action EndTurn), (opt, origin)))

and domestic_trade (((_, plist, t, _) as s)) stage opt origin mar =
  let ((c, (inv, hand), (ks, lr, la)), rest) = get_player t.active plist in
  let cost = stage_cost stage in
  let (have, want) = account inv cost in
  let f (resource, plist) (c, (inv, _), _) =
    if enough_resources inv (single_resource_cost resource)
    then (resource, ((c, resource) :: plist))
    else (resource, plist) in
  let g plist resource =
    let inters = List.fold_left f (resource, []) rest in (snd inters) @ plist in
  let possible = List.fold_left g [] want in
  let f (_, h1) (_, h2) = - (h1 - h2) in
  let sorted = List.sort f have
  in
    match (sorted, possible) with
    | ([], _) -> maritime_trade s stage opt origin mar
    | (_, []) -> maritime_trade s stage opt origin mar
    | _ ->
        let (res, _) = List.hd sorted in
        let ((c, resource), _) = pick_one possible in
        let give = single_resource_cost res in
        let gain = single_resource_cost resource
        in ((Action (DomesticTrade (c, give, gain))), (opt, origin))

and handle s orig opt origin mar =
  let rec helper (((board, plist, t, (colour, _)) as s)) stage opt =
    let ((c, (inv, hand), (ks, lr, la)), l) = get_player t.active plist in
    let (card, (opt1, origin)) = play_card s (reveal hand) stage opt origin
    in
      match ((t.cardplayed), (is_none card)) with
      | (false, false) ->
          ((Action (PlayCard (get_some card))), (opt1, origin))
      | _ ->
          let enough = enough_resources inv (stage_cost stage) in
          let allowed = t.tradesmade < cNUM_TRADES_PER_TURN in
          let traded = t.tradesmade > 0
          in
            if is_none t.dicerolled
            then ((Action RollDice), (opt, origin))
            else
              (match (enough, allowed, (stage = 4), traded, mar) with
               | (_, _, _, _, true) -> maritime_trade s orig opt origin mar
               | (true, _, _, _, _) -> build s stage opt origin mar
               | (_, _, false, false, _) ->
                   let next = if stage = 2 then 4 else stage + 1 in
                   let next = if lr && (next = 1) then 2 else next
                   in helper s next opt
               | (_, true, _, _, _) -> domestic_trade s orig opt origin mar
               | (_, false, _, true, _) ->
                   maritime_trade s orig opt origin mar
               | _ -> ((Action EndTurn), (opt, origin)))
  in helper s orig opt

and build (((board, plist, t, (_, _)) as s)) stage opt origin mar =
  let ((c, (inv, hand), (ks, lr, la)), rest) = get_player t.active plist
  in
    match stage_cost stage with
    | n when n = cCOST_TOWN -> build_town s stage opt origin mar
    | n when n = cCOST_CITY -> ((build_city s), (opt, origin))
    | n when n = cCOST_ROAD ->
        let ((r1, _, opt), origin) = build_road t.active s opt origin
        in
          (match r1 with
           | None ->
               let next = if stage = 2 then 4 else stage + 1 in
               let next = if lr && (next = 1) then 2 else next
               in handle s next opt origin mar
           | Some r -> ((Action (BuyBuild (BuildRoad r))), (opt, origin)))
    | _ -> ((Action (BuyBuild BuildCard)), (opt, origin))

and build_town (((board, plist, t, (_, _)) as s)) stage opt origin mar =
  let (_, (insecs, roads), _, _, _) = board in
  let mine = get_player_roads t.active roads
  in
    let module Aset =
      Set.Make(struct let compare = Pervasives.compare

                         type t = point
                          end)
    in
      let f set (_, (p1, p2)) = let s1 = Aset.add p1 set in Aset.add p2 s1 in
      let set = List.fold_left f Aset.empty mine in
      let potentials = Aset.elements set in
      let f x = valid_town_build t.active x (insecs, roads) in
      let safes = List.filter f potentials
      in
        match safes with
        | [] -> handle s 1 opt origin mar
        | _ ->
            let (p, _) = pick_one safes
            in ((Action (BuyBuild (BuildTown p))), (opt, origin))
  
