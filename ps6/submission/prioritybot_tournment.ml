(* 
  Authors: 
    Humam Alwassel (haa54) & Solomon Maina (sam524)
 *)

open Definition
open Registry
open Constant
open Util
open Print

(* ======================================================================= *)
(* ========================== START: BotUtil ============================= *)
(* ======================================================================= *)
 
(*******************************************************)
(*********  string representation of types *************)
(*******************************************************)
let string_of_hex (tar, roll) =
  (match tar with
   | Hill -> "(Hill, "
   | Pasture -> "(Pasture, "
   | Mountain -> "(Mountain, "
   | Field -> "(Field, "
   | Forest -> "(Forest, "
   | Desert -> "(Desert, ") ^ ((string_of_int roll) ^ ")")
  
let string_of_resources (d1, d2, d3, d4, d5) =
  "(" ^
    ((string_of_int d1) ^
       ("," ^
          ((string_of_int d2) ^
             ("," ^
                ((string_of_int d3) ^
                   ("," ^
                      ((string_of_int d4) ^
                         ("," ^ ((string_of_int d5) ^ ")")))))))))
  
let string_of_color color =
  match color with
  | Blue -> "Blue "
  | Red -> "Red "
  | Orange -> "Orange "
  | White -> "White "
  
let string_of_settlement settl =
  match settl with | Town -> "Town " | City -> "City "
  
let string_of_intersection inter =
  if is_none inter
  then "[no settlement]"
  else
    (let (color, settl) = get_some inter
     in
       "[" ^
         ((string_of_color color) ^
            (": " ^ ((string_of_settlement settl) ^ "]"))))
  
let string_of_road (c1, (p1, p2)) : string =
  "(" ^
    ((string_of_color c1) ^ (" (" ^ ((soi p1) ^ ("," ^ ((soi p2) ^ "))")))))
  
(*******************************************************)
(*******************************************************)
(* return the intersections corresponding to the a points in plst. In      *)
(* output, intersections are ordered by increasing based on their point    *)
(* number                                                                  *)
let get_intersections_from_points (plst : point list)
  (ilst : intersection list) : intersection list =
  let rec helper out plst ilst nth =
    match (plst, ilst, nth) with
    | ([], _, _) -> List.rev out
    | (ph :: pt, hh :: ht, n) ->
        if ph = n
        then helper (hh :: out) pt ht (n + 1)
        else helper out plst ht (n + 1)
    | _ ->
        failwith
          "<get_intersections_from_points> entries in plst don't map to entries in ilst"
  in helper [] (List.sort compare plst) ilst 0
  
(* return a higher int value if the probability of rolling this roll is    *)
(* higher                                                                  *)
let get_roll_probablity =
  function
  | 6 | 8 -> 5. /. 36.
  | 5 | 9 -> 4. /. 36.
  | 4 | 10 -> 3. /. 36.
  | 3 | 11 -> 2. /. 36.
  | 2 | 12 -> 1. /. 36.
  | _ -> 0.
  
(* = 0 if two rolls have equal probability. > 0 if the roll1 has more      *)
(* probability. < 0 if roll2 has more probability                          *)
let compare_rolls roll1 roll2 =
  (get_roll_probablity roll1) -. (get_roll_probablity roll2)
  
(* populate the point_pieces hashtable. *)
let populate_point_pieces_hashtable table =
  let rec helper point =
    if point < cNUM_POINTS
    then
      (Hashtbl.add table point (adjacent_pieces point); helper (point + 1))
    else ()
  in helper cMIN_POINT_NUM
  
(* populate the piece_hex hashtable. *)
let populate_piece_hex_hashtable table hex_list =
  let f acc hex = (Hashtbl.add table acc hex; acc + 1) in
  let _ = List.fold_left f cMIN_PIECE_NUM hex_list in ()
  
(* get a list of points that border 3 pieces and those pieces have higher  *)
(* roll probability return a list of tuples of (point, evaluation stats).  *)
(* The list is sorted by which point have higher evaluation                *)
let get_first_town_options point_pieces piece_hex =
  let f point adj_pieces out =
    if (List.length adj_pieces) < 3
    then out
    else
      (let helper (prob_lst, sum) ele =
         let prob = get_roll_probablity (snd (Hashtbl.find piece_hex ele))
         in ((prob :: prob_lst), (prob +. sum))
       in (point, (List.fold_left helper ([], 0.) adj_pieces)) :: out) in
  let lst = Hashtbl.fold f point_pieces [] in
  let my_compare (p2, (prob_lst2, sum2)) (p1, (prob_lst1, sum1)) =
    if (compare sum1 sum2) <> 0
    then compare sum1 sum2
    else
      compare (List.fold_left max 0. prob_lst1)
        (List.fold_left max 0. prob_lst2)
  in List.sort my_compare lst
  
(* true iff p1 is at least two roads away from any other settlement and p1 *)
(* does not have a settlement                                              *)
let is_valid_town inter_list p1 =
  let rec helper =
    function
    | [] -> true
    | h :: t -> (is_none (List.nth inter_list h)) && (helper t)
  in
    (p1 >= cMIN_POINT_NUM) &&
      ((p1 <= cMAX_POINT_NUM) &&
         ((is_none (List.nth inter_list p1)) && (helper (adjacent_points p1))))
  
(* get a list of points that border a port and 2 pieces and those pieces   *)
(* have higher roll probability, and the port has a good trade ratio.      *)
(* return a list of tuples of (point, evaluation stats). The list is       *)
(* sorted by which point have higher evaluation                            *)
let get_second_town_options point_pieces piece_hex (port_list : port list)
                            resources_from_first_town =
  let f out ((p1, p2), ratio, kind) =
    let adj_pieces1 = adjacent_pieces p1

    and adj_pieces2 = adjacent_pieces p2 in
    let helper (prob_lst, sum) ele =
      let prob = get_roll_probablity (snd (Hashtbl.find piece_hex ele))
      in ((prob :: prob_lst), (prob +. sum)) in
    let trade_points = (abs (4 - ratio)) + (if kind = Any then 1 else 0) in
    let (lst1, lst2) =
      ((List.map
          (fun piece ->
             resource_of_terrain (fst (Hashtbl.find piece_hex piece)))
          (adjacent_pieces p1)),
       (List.map
          (fun piece ->
             resource_of_terrain (fst (Hashtbl.find piece_hex piece)))
          (adjacent_pieces p2))) in
    let (terrain_points1, terrain_points2) =
      ((list_count (fun x -> not (List.mem x resources_from_first_town)) lst1),
       (list_count (fun x -> not (List.mem x resources_from_first_town)) lst2)) in
    let (prob_lst1, sum1) = List.fold_left helper ([], 0.) adj_pieces1

    and (prob_lst2, sum2) = List.fold_left helper ([], 0.) adj_pieces2
    in
      (p1,
       (prob_lst1,
        ((sum1 +. (float trade_points)) +. (float terrain_points1)))) ::
        (p2,
         (prob_lst2,
          ((sum2 +. (float trade_points)) +. (float terrain_points2)))) ::
        out in
  let lst = List.fold_left f [] port_list in
  let my_compare (p2, (prob_lst2, sum2)) (p1, (prob_lst1, sum1)) =
    if (compare sum1 sum2) <> 0
    then compare sum1 sum2
    else
      compare (List.fold_left max 0. prob_lst1)
        (List.fold_left max 0. prob_lst2)
  in List.sort my_compare lst
  
(* return a tuple (x, xs) where x is the player with color c and xs is a   *)
(* list of the rest of the player                                          *)
let get_player c player_list =
  let (x, xs) = List.partition (fun (color, _, _) -> color = c) player_list
  in
    match x with
    | [ y ] -> (y, xs)
    | _ ->
        let len = List.length x
        in
          failwith
            (" < get_player > player with the color provided
          is not present (or present multiple times) in player_list."
               ^ ("player is present " ^ ((string_of_int len) ^ " times.")))
  
(** Returns the number of victory point associated with the type of settlement *)
let settlement_victory_point (set : settlement) : int =
  match set with | Town -> cVP_TOWN | City -> cVP_CITY
  
let get_num_hidden player =
  function
  | Hidden x -> x
  | _ -> failwith ("<get_num_hidden>" ^ string_of_color player ^ "'s cards are revealed")
  
let get_player_vpoints player_list player inters =
  let ((_, (_, cards), (_, longestroad, largestarmy)), _) =
    get_player player player_list in
  let (points_from_settlement, settlements) =
    let rec helper (out, settl_lst) index =
      function
      | [] -> (out, settl_lst)
      | h :: t ->
          if (is_none h) || (( != ) (fst (get_some h)) player)
          then helper (out, settl_lst) (index + 1) t
          else
            helper
              ((out + (settlement_victory_point (snd (get_some h)))),
               (index :: settl_lst))
              (index + 1) t
    in helper (0, []) 0 inters in
  let points_from_trophies =
    (if longestroad then cVP_LONGEST_ROAD else 0) +
      (if largestarmy then cVP_LARGEST_ARMY else 0) in
  let hidden_num = get_num_hidden player cards in
  let sum_points = points_from_settlement + points_from_trophies
  in (sum_points, hidden_num, settlements)
  
let get_opponents_vpoints player_list myColor inters =
  let players = [ Blue; Red; Orange; White ] in
  let f acc elem =
    if elem = myColor
    then acc
    else (elem, (get_player_vpoints player_list elem inters)) :: acc in
  let lst = List.fold_left f [] players in
  let my_compare (_, (vp2, hidden_num2, _)) (_, (vp1, hidden_num1, _)) =
    if (compare vp1 vp2) <> 0
    then compare vp1 vp2
    else compare hidden_num1 hidden_num2
  in List.sort my_compare lst
  
(* true iff player c has a settlement neighboring piece p *)
let has_settlement_around_piece p c inter_list =
  let f x = (not (is_none x)) && ((fst (get_some x)) = c)
  in
    (list_count f
       (get_intersections_from_points (piece_corners p) inter_list))
      > 0
  
(* return a tuple (x, xs) where x is the player with color c and xs is a   *)
(* list of the rest of the player                                          *)
let get_player c player_list =
  let (x, xs) = List.partition (fun (color, _, _) -> color = c) player_list
  in
    match x with
    | [ y ] -> (y, xs)
    | _ ->
        let len = List.length x
        in
          failwith
            (" < get_player > player with the color provided
          is not present (or present multiple times) in player_list."
               ^ ("player is present " ^ ((string_of_int len) ^ " times.")))
  
(* = resource1 + resource2 *)
let plus_resources resource1 resource2 = map_cost2 ( + ) resource1 resource2
  
(* = resource1 - resource2 *)
let subtract_resources resource1 resource2 =
  map_cost2 ( - ) resource1 resource2
  
(* returns number of settlement of type t that belong to player c *)
let num_settlements c t inter_list =
  let f x =
    (not (is_none x)) &&
      (((fst (get_some x)) = c) && ((snd (get_some x)) = t))
  in list_count f inter_list
  
let cost_fold2 : ('a -> int -> int -> 'a) -> 'a -> cost -> cost -> 'a =
  fun f v (b, w, o, l, g) (b', w', o', l', g') ->
    f (f (f (f (f v b b') w w') o o') l l') g g'
  
let enough_resources : cost -> cost -> bool =
  fun cost1 cost2 ->
    let f bool n1 n2 = bool && (n1 >= n2) in cost_fold2 f true cost1 cost2
 
let player_settlements_built :
  color -> settlement -> intersection list -> int =
  fun c setl inter_list ->
    let f x =
      (not (is_none x)) && (let (a, b) = get_some x in (a = c) && (b = setl))
    in list_count f inter_list
  
let stage_cost : int -> cost =
  function
  | 0 -> cCOST_CITY
  | 1 -> cCOST_ROAD
  | 2 -> cCOST_TOWN
  | 3 -> cCOST_CITY
  | _ -> cCOST_CARD
  
let least_ratio :
  color -> port list -> intersection list -> resource -> ratio =
  fun c ports l resource ->
    let f n ((a, b), r, res) =
      let res = match res with | Any -> resource | PortResource r' -> r' in
      let (p1, p2) = ((List.nth l a), (List.nth l b))
      in
        match ((not (is_none p1)), (not (is_none p2))) with
        | (true, true) ->
            let ((c1, _), (c2, _)) = ((get_some p1), (get_some p2))
            in
              if ((c1 = c) || (c2 = c)) && ((r < n) && (resource = res))
              then r
              else n
        | (true, false) ->
            let (c1, _) = get_some p1
            in if (c1 = c) && ((r < n) && (resource = res)) then r else n
        | (false, true) ->
            let (c2, _) = get_some p2
            in if (c2 = c) && ((r < n) && (resource = res)) then r else n
        | (false, false) -> n
    in List.fold_left f cMARITIME_DEFAULT_RATIO ports
  
let get_player_roads (c : color) (roads : road list) : road list =
  fst (List.partition (fun (x, _) -> x = c) roads)
  
let touches_player_road : color -> point -> road list -> bool =
  fun colour p roads ->
    let f bool (c, (p1, p2)) =
      bool || ((c = colour) && ((p = p1) || (p = p2)))
    in List.fold_left f false roads
  
let valid_town_build : color -> point -> structures -> bool =
  fun colour p structs ->
    let (insecs, roads) = structs
    in (is_valid_town insecs p) && (touches_player_road colour p roads)
  
let is_valid_line (p1, p2) =
  (p1 >= cMIN_POINT_NUM) &&
    ((p1 <= cMAX_POINT_NUM) &&
       ((List.length (List.filter (fun x -> x = p2) (adjacent_points p1))) =
          1))
  
let road_not_bought : road -> road list -> bool =
  fun (_, (p1, p2)) roads ->
    let f (_, (p3, p4)) = ((p1, p2) <> (p3, p4)) && ((p1, p2) <> (p4, p3))
    in List.for_all f roads
  
let valid_road_build : road -> road list -> intersection list -> bool =
  fun (((c1, (p1, p2)) as road)) roads insecs ->
    match (is_valid_line (p1, p2)) && (road_not_bought road roads) with
    | true ->
        let f bool (c2, (p3, p4)) =
          (match ((p1 = p3), (p1 = p4), (p2 = p3), (p2 = p4)) with
           | (true, _, _, _) | (_, true, _, _) ->
               let not_enemy =
                 let setl = List.nth insecs p1
                 in (is_none setl) || ((fst (get_some setl)) = c1) in
               let valid = (c1 = c2) && not_enemy in bool || valid
           | (_, _, true, _) | (_, _, _, true) ->
               let not_enemy =
                 let setl = List.nth insecs p2
                 in (is_none setl) || ((fst (get_some setl)) = c1) in
               let valid = (c1 = c2) && not_enemy in bool || valid
           | _ -> bool)
        in List.fold_left f false roads
    | false -> false

let n_of_resource : int -> resource -> cost = 
  fun n resource ->
    match resource with
    | Brick -> (n,0,0,0,0)
    | Wool -> (0,n,0,0,0)
    | Ore -> (0,0,n,0,0)
    | Grain -> (0,0,0,n,0)
    | Lumber -> (0,0,0,0,n)
let sop (p1, p2) = "("^(soi p1)^","^(soi p2)^")"

let sor (c1, (p1, p2)) = "("^( string_of_color c1)^ "," ^ (sop (p1, p2))^")"

(* ======================================================================= *)
(* =========================== END: BotUtil ============================== *)
(* ======================================================================= *)


(* ======================================================================= *)
(* ======================= START: TradeRequestBot ======================== *)
(* ======================================================================= *)

module TradeRequestBot = 
  struct 
    let enables_to_build : cost -> int -> bool =
      fun inv stage ->
      let cost = match stage with
        | 0 -> cCOST_CITY
        | 1 -> cCOST_ROAD
        | 2 -> cCOST_TOWN
        | 3 -> cCOST_CITY
        | _ -> cCOST_CARD
      in 
        let (r1,r2,r3,r4,r5) = subtract_resources inv cost in
        r1>=0 && r2>=0 && r3>=0 && r4>=0 && r5>=0
      

    (* TradeRequest: have two functions enable_to_bulid and is_fair.       
    enable_to_bulid returns the value of "this trades will allow us   
    to build something (a town/city or a road)", and is_fair returns 
    true iff [resources we get] - [resources we give] >= -1. Accept 
    a trade iff enables_to_build && is_fair *)
    let handle : state -> int -> move =
      fun (board, plist, turn, (colour, _)) stage ->
        let ((c, (inv, hand), (ks, lr, la)), l) = get_player colour plist in
        let (_, c1, c2) = get_some turn.pendingtrade in
        let after = plus_resources c1 (subtract_resources inv c2) in
        let fair = ((sum_cost inv) - (sum_cost after)) >= (-1)
        in
          if (enables_to_build after stage) && fair
          then TradeResponse true else TradeResponse false
end
  

(* ======================================================================= *)
(* ======================= END: TradeRequestBot ========================== *)
(* ======================================================================= *)

(* ======================================================================= *)
(* ===================== START: ActionRequestBot ========================= *)
(* ======================================================================= *)

module ActionRequestBot = struct 

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
    try
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
    with | _ -> ((Action EndTurn), (opt, origin))

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
end  

(* ======================================================================= *)
(* ====================== END: ActionRequestBot ========================== *)
(* ======================================================================= *)






(** Give your bot a 2-20 character name. *)
let name = "prioritybot"

module Bot = functor (S : Soul) -> struct
				(* If you use side effects, start/reset your bot for a new game *)

(* stages refer to highest priority. If you can't meet this priority, then *)
(* try to do something else. E.g. in stage 0, if you can't upgrade a town  *)
(* to a city, then try and build a road. stage 0: convert 2 towns to 2     *)
(* cities. stage 1: build roads to get longest road trophy stage 2: build  *)
(* an extra town. stage 3: convert third town to city. stage 4: buy cards  *)
(* (largest army?).                                                        *)
let stage = ref 0 
(* the resources we care about the most in this stage. Update it whenever  *)
(* you switch stages                                                       *)
let resources_in_interest = ref []
let point_pieces = Hashtbl.create cNUM_POINTS
let piece_hex = Hashtbl.create cNUM_PIECES
let first_move = ref true
let myColor = ref White
let resources_from_first_town = ref []
let goal : (point * point) option ref = ref None
let origin : (point * point) option ref = ref None
let mar : bool ref = ref false

(* update resources_in_interest to include the types of resources needed   *)
(* for cost                                                                *)
let update_resources_in_interest cost = 
  let (b, w, o, g, l) = cost in 
  let lst = List.combine [b; w; o; g; l] [Brick; Wool; Ore; Grain; Lumber] in
    resources_in_interest := 
      List.fold_left (fun acc (i, t) -> if i > 0 then t:: acc else acc) [] lst

(* takes player list and intersection list. Checks whether the goal of the *)
(* current stage is met, and updates the value of stage and                *)
(* resources_in_interest if necessary.                                     *)
let update_stage_and_resources_in_interest player_list inter_list = 
(* stage 0: convert 2 towns to 2 cities. => resources_in_interest =        *)
(* cCOST_CITY stage 1: get longest road trophy. => resources_in_interest = *)
(* cCOST_ROAD stage 2: build an extra town. => resources_in_interest =     *)
(* cCOST_TOWN stage 3: convert third town to city. =>                      *)
(* resources_in_interest = cCOST_CITY stage 4: buy cards, get largest      *)
(* army. => resources_in_interest = cCOST_CARD                             *)
  let ((_, _, (_, longestroad, _)), _) = get_player (!myColor) player_list in
  let num_cities = num_settlements (!myColor) City inter_list in
  let num_towns = num_settlements (!myColor) Town inter_list in
  match (!stage), longestroad with
  | 0, _ -> 
		if num_cities = 2 then (stage := 1; update_resources_in_interest cCOST_ROAD)
    else (stage := 0; update_resources_in_interest cCOST_CITY)
  | 1, true -> (stage := 2; update_resources_in_interest cCOST_TOWN)
	| _, false -> (stage := 1; update_resources_in_interest cCOST_ROAD)
  | 2,_ -> 
		if num_towns = 1 then (stage := 3; update_resources_in_interest cCOST_CITY) 
		else () 
  | 3,_ -> 
		if num_cities = 3 then (stage := 4; update_resources_in_interest cCOST_CARD)
		else ()
  | _ -> ()



(* first town: should be near adjacent to 3 pieces, and those three        *)
(* adjacent pieces have high probability roll (i.e.                        *)
(* 8=6>9=5>10=4>11=3>12=2). second town: should be next to a port with     *)
(* good trading discount ratio, and border pieces with high probability    *)
(* roll. If possible, it should also be next to pieces that generate       *)
(* resources that the first town can't obtain                              *)
let rec handle_InitialRequest (((map, structures, _, _, _), _, _, (c, _)) as s : state) : move = 
  if !first_move then begin
    let () = first_move:= false in
    let () = myColor := c in
    let () = populate_piece_hex_hashtable piece_hex (fst map) in
    let options = get_first_town_options point_pieces piece_hex in
    let p = 
      (try 
        fst (List.find (fun (p, _) -> is_valid_town (fst structures) p) options )
      with _ -> Random.int cNUM_POINTS)
    in
    let lst = 
      List.map (fun piece -> resource_of_terrain (fst (Hashtbl.find piece_hex piece))) (adjacent_pieces p)
    in 
    let () = resources_from_first_town := lst in 
      InitialMove (p, get_some (pick_random (adjacent_points p)))
  end
  else begin
    let options = get_second_town_options point_pieces piece_hex (snd map) (!resources_from_first_town) 
    in
(* let () = List.fold_left (fun () (p, (_, sum)) -> print_endline ("####(" *)
(* ^ string_of_int p ^ ", " ^ string_of_float sum ^ ") ")) () options in   *)
    try
      let (p1, _) = List.find (fun (p, _) -> is_valid_town (fst structures) p) options in
      InitialMove (p1, get_some (pick_random (adjacent_points p1)))
    with _ -> begin
      first_move := true;
      handle_InitialRequest s
    end
  end

(* RobberRequest: first thing make sure that you don't move the robber to  *)
(* a piece next to our town or city. Robber should steal from the opponent *)
(* with the highest victory points at the time. calculate victory points   *)
(* by checking towns and cities and trophies. If there is no unique        *)
(* opponent with highest victory points, then pick the opponents that has  *)
(* more hidden cards. once you identify a player to steal from, go look    *)
(* for a piece that boards any of his settlements, and place the robber    *)
(* there. Make sure that the piece you pick at the end is not a piece next *)
(* to our settlements. If you can't find any piece where this condition is *)
(* satisfied, then pick the opponent with the next highest victory points  *)
(* and do the same again.                                                  *)
let handle_RobberRequest (((map, structures, _, _, robber), pl, _, _) : state) : move =
  let options = get_opponents_vpoints pl (!myColor) (fst structures) in 
  let rec helper = function
    | [] -> None
    | (color, (vp, hidden_num, settl)):: t -> begin 
      let pieces = List.flatten (List.map (adjacent_pieces) settl) in
      let f p = p <> robber && not (has_settlement_around_piece p (!myColor) (fst structures)) in
      try
        Some (List.find f pieces, Some color)
      with _ -> helper t
    end
  in
  match helper options with 
  | Some x -> RobberMove x
  | None -> begin 
    let rec get_random_piece () =
      let p = Random.int cNUM_PIECES in 
      if p <> robber && not (has_settlement_around_piece p (!myColor) (fst structures))
      then RobberMove (p, None)
      else get_random_piece () 
    in get_random_piece ()
  end 

(* Discard request: look in the resources_in_interest list and try to keep *)
(* as much as you can of those and discard the other kinds of resources    *)
let handle_DiscardRequest (((_, _, _, _, _), pl, _, _) : state) : move =
  let ((_, ((b, w, o, g, l)as inv, _), _), _) = get_player (!myColor) pl in 
  let half = (sum_cost inv) / 2 in 
  let rec f out (count, kind) = 
    if count <= 0 then out else f (kind:: out) (count -1, kind) 
  in
  let all = 
    List.flatten (List.map (f []) (List.combine [b; w; o; g; l] [Brick; Wool; Ore; Grain; Lumber]))
  in
  let l1, l2 = 
    List.partition (fun x -> List.mem x (!resources_in_interest)) all 
  in
  let in_intereset, not_in_interest = (randomize l1, randomize l2) in 
  let rec helper out lst1 lst2 =
    if sum_cost out = half then out 
    else begin
      match lst1, lst2 with 
      | h:: t, _ -> helper (plus_resources out (single_resource_cost h)) t lst2
      | [], h:: t -> helper (plus_resources out (single_resource_cost h)) lst1 t 
      | _ -> (0,0,0,0,0)
    end 
  in
  let cost = helper (0,0,0,0,0) not_in_interest in_intereset
  in
    DiscardMove cost

  let initialize () =
    let () = first_move := true in
    let () = stage := 0 in
    let () = update_resources_in_interest cCOST_CITY in
    let () = Hashtbl.reset point_pieces in
    let () = populate_point_pieces_hashtable point_pieces in 
    let () = Hashtbl.reset piece_hex in
    let () = resources_from_first_town := [] in
    let () = goal := None in
    let () = origin := None in
		let () = mar := false in
    ()

	(* Invalid moves are overridden in game *)
  let handle_request ((b, p, t, (c, r)) as s : state) : move = 
		let (_, (inter_list, _), _, _, _) = b in
		let () = update_stage_and_resources_in_interest p inter_list in
    match r with
      | InitialRequest -> handle_InitialRequest s
      | RobberRequest -> handle_RobberRequest s
      | DiscardRequest -> DiscardMove(0,0,0,0,0)
      | TradeRequest -> TradeRequestBot.handle s (!stage)
      | ActionRequest -> let m, (opt, orig) = 
				ActionRequestBot.handle s (!stage) (!goal) (!origin) (!mar) in 
				let () = goal := opt in let () = origin := orig in 
				match m with
				| Action (MaritimeTrade (_)) -> let () = mar := true in m
				| _ -> let () = mar := false in m
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))