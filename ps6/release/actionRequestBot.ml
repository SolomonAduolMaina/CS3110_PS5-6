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
          else
            if v1 >= v2
            then ((n + 1), l1, l2)
            else ((n + 1), l1, (Brick :: l2))
      | 1 ->
          if v2 = 0
          then ((n + 1), ((Wool, v1) :: l1), l2)
          else
            if v1 >= v2
            then ((n + 1), l1, l2)
            else ((n + 1), l1, (Wool :: l2))
      | 2 ->
          if v2 = 0
          then ((n + 1), ((Ore, v1) :: l1), l2)
          else
            if v1 >= v2
            then ((n + 1), l1, l2)
            else ((n + 1), l1, (Ore :: l2))
      | 3 ->
          if v2 = 0
          then ((n + 1), ((Grain, v1) :: l1), l2)
          else
            if v1 >= v2
            then ((n + 1), l1, l2)
            else ((n + 1), l1, (Grain :: l2))
      | _ ->
          if v2 = 0
          then ((n + 1), ((Lumber, v1) :: l1), l2)
          else
            if v1 >= v2
            then ((n + 1), l1, l2)
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
  let sorted = List.sort f assoc in Some (fst (List.hd sorted))
  
let road_points : road list -> point list =
  fun roads -> List.concat (List.map (fun (_, (p1, p2)) -> [ p1; p2 ]) roads)
  
let continue points (((board, plist, t, next) as s)) =
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
    | None -> (None, s, None)
    | Some road ->
        let roads = road :: roads in
        let board = (a1, (insecs, roads), a2, a3, a4) in
        let s = (board, plist, t, next) in ((Some road), s, None)
  
let rec which_road path points point =
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
            | _ -> which_road (y :: xs) points point)
       | (false, false) -> failwith "Nope"
       | (false, true) -> which_road (y :: xs) points point)
  | [ x ] -> (None, None)
  
let rec best_route colour (((board, plist, turn, next) as s)) opt =
  let (a1, (insecs, roads), a2, a3, a4) = board
  in
    match opt with
    | None ->
        let () = print "Hapa hivi" in
        let mine = get_player_roads turn.active roads
        in
          (match (List.length mine) = 2 with
           | true ->
               let (_, l1) = List.hd mine in
               let (_, l2) = List.hd (List.tl mine) in
               let road = best_shortest colour l1 l2 insecs
               in best_route turn.active s road
           | false -> continue (road_points mine) s)
    | Some (p1, p2) ->
        let queue = Queue.create () in
        let map = PMap.empty in
        let () = Queue.add p1 queue in
        let map = PMap.add p1 [] map in
        let path = shortest_path turn.active p2 insecs queue map in
        let points = road_points (get_player_roads turn.active roads)
        in
          (match which_road path points p2 with
           | (None, _) -> failwith "actually here"
           | (Some (x, y), None) ->
               let roads = ((turn.active), (x, y)) :: roads in
               let board = (a1, (insecs, roads), a2, a3, a4) in
               let s = (board, plist, turn, next) in
               let road = Some (turn.active, (x, y)) in (road, s, None)
           | (Some (x, y), q) ->
               let roads = ((turn.active), (x, y)) :: roads in
               let board = (a1, (insecs, roads), a2, a3, a4) in
               let s = (board, plist, turn, next) in
               let road = Some (turn.active, (x, y)) in (road, s, q))
  
let build_road (((_, _, t, _) as s)) opt = best_route t.active s opt
  
let what_card state card stage opt =
  match card with
  | Knight -> play_knight state
  | RoadBuilding ->
      let (r1, s, opt1) = build_road state opt in
      let (r2, _, _) = build_road s opt1
      in
        (match ((is_none r1), (is_none r2)) with
         | (true, _) | (_, true) -> None
         | _ ->
             let (r1, r2) = ((get_some r1), (get_some r2))
             in Some (PlayRoadBuilding (r1, (Some r2))))
  | YearOfPlenty -> play_plenty state stage
  | Monopoly -> play_monopoly state stage
  | _ -> failwith "No way"
  
let rec play_card state hand stage opt =
  match hand with
  | [] -> None
  | card :: cards ->
      let playcards = [ Knight; RoadBuilding; YearOfPlenty; Monopoly ]
      in
        (match List.mem card playcards with
         | true ->
             let card = what_card state card stage opt
             in
               (match is_none card with
                | false -> card
                | true -> play_card state cards stage opt)
         | false -> play_card state cards stage opt)
  
let rec
  domestic_trade (((board, plist, turn, (colour, _)) as s)) stage mar dom opt
                 =
  let ((c, (inv, hand), (ks, lr, la)), rest) = get_player colour plist in
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
    | ([], _) -> handle s stage mar true opt
    | (_, []) -> handle s stage mar true opt
    | _ ->
        let (res, _) = List.hd sorted in
        let ((c, resource), _) = pick_one possible in
        let give = single_resource_cost res in
        let gain = single_resource_cost resource
        in ((Action (DomesticTrade (c, give, gain))), opt)

and maritime_trade (((board, plist, turn, (colour, _)) as s)) stage dom opt =
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
    | None -> handle s stage true dom opt
    | Some (res, _) ->
        let (gain, _) = pick_one want
        in ((Action (MaritimeTrade (res, gain))), opt)

and handle (((board, plist, turn, (colour, _)) as s)) stage mar dom opt =
  let () = print ("Handling stage " ^ (soi stage)) in
  let ((c, (inv, hand), (ks, lr, la)), l) = get_player colour plist in
  let card = play_card s (reveal hand) stage opt
  in
    match ((turn.cardplayed), (is_none card)) with
    | (false, false) -> ((Action (PlayCard (get_some card))), opt)
    | _ ->
        let enough = enough_resources inv (stage_cost stage) in
        let limit = cNUM_TRADES_PER_TURN
        in
          if is_none turn.dicerolled
          then ((Action RollDice), opt)
          else
            (match (enough, (turn.tradesmade)) with
             | (true, _) -> build s stage mar dom opt
             | (false, n) when n < (limit / 2) ->
                 (match (mar, dom) with
                  | (true, true) -> ((Action EndTurn), opt)
                  | (true, false) -> domestic_trade s stage mar dom opt
                  | (false, true) -> maritime_trade s stage dom opt
                  | (false, false) -> domestic_trade s stage mar dom opt)
             | (false, n) when (n >= (limit / 2)) && (n < limit) ->
                 (match (mar, dom) with
                  | (true, true) -> ((Action EndTurn), opt)
                  | (true, false) -> domestic_trade s (stage + 1) mar dom opt
                  | (false, true) -> maritime_trade s (stage + 1) dom opt
                  | (false, false) ->
                      domestic_trade s (stage + 1) mar dom opt)
             | _ ->
                 if not mar
                 then maritime_trade s stage dom opt
                 else ((Action EndTurn), opt))

and build (((board, plist, turn, (_, _)) as s)) stage mar dom opt =
  match stage_cost stage with
  | n when n = cCOST_TOWN -> build_town s stage mar dom opt
  | n when n = cCOST_CITY -> ((build_city s), opt)
  | n when n = cCOST_ROAD ->
      let (r1, _, opt) = build_road s opt
      in
        ((match r1 with
          | None -> fst (handle s (stage + 1) mar dom opt)
          | Some road -> Action (BuyBuild (BuildRoad road))),
         opt)
  | _ -> ((Action (BuyBuild BuildCard)), opt)

and build_town (((board, plist, turn, (_, _)) as s)) stage mar dom opt =
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
        | [] -> handle s stage mar dom opt
        | _ ->
            let (p, _) = pick_one safes
            in ((Action (BuyBuild (BuildTown p))), opt)
  
