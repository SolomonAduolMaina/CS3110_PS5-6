open Definition
open Constant
open Util
open Print

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
    | Desert -> "(Desert, ") ^ (string_of_int roll) ^ ")"

let string_of_resources (d1, d2, d3, d4, d5) =
  "(" ^ (string_of_int d1) ^","^ (string_of_int d2) ^","^
  (string_of_int d3) ^","^ (string_of_int d4) ^","^ (string_of_int d5) ^")"

let string_of_color color =
  match color with
  | Blue -> "Blue "
  | Red -> "Red "
  | Orange -> "Orange "
  | White -> "White "

let string_of_settlement settl =
  match settl with
  | Town -> "Town "
  | City -> "City "

let string_of_intersection inter =
  if is_none inter then "[no settlement]" else
    let (color, settl) = get_some inter in
    "[" ^ (string_of_color color) ^ ": " ^ (string_of_settlement settl) ^ "]"

let string_of_road (c1, (p1, p2)) : string =
  "(" ^ (string_of_color c1) ^ " (" ^ (soi p1) ^ "," ^ ((soi p2) ^ "))")

(*******************************************************)
(*******************************************************)

(* return the intersections corresponding to the a points in plst. In      *)
(* output, intersections are ordered by increasing based on their point    *)
(* number                                                                  *)
let get_intersections_from_points (plst : point list) (ilst : intersection list): intersection list =
  let rec helper out plst ilst nth =
    match plst, ilst, nth with
    | [], _, _ -> List.rev out
    | ph:: pt, hh:: ht, n -> begin
          if ph = n then helper (hh:: out) pt ht (n +1)
          else helper out plst ht (n +1)
        end
    | _ -> failwith "<get_intersections_from_points> entries in plst don't map to entries in ilst"
  in
  helper [] (List.sort (compare) plst) ilst 0


(* return a higher int value if the probability of rolling this roll is higher *)
let get_roll_probablity = function
	| 6 | 8  -> 5./.36.
	| 5 | 9  -> 4./.36. 
	| 4 | 10 -> 3./.36.
	| 3 | 11 -> 2./.36.
	| 2 | 12 -> 1./.36.
	| _      -> 0.

(* = 0 if two rolls have equal probability. 
   > 0 if the roll1 has more probability.
   < 0 if roll2 has more probability *)
let compare_rolls roll1 roll2 =
  get_roll_probablity roll1 -. get_roll_probablity roll2

(* populate the point_pieces hashtable. *)
let populate_point_pieces_hashtable table =
  let rec helper point =
    if point < cNUM_POINTS then begin
      Hashtbl.add table point (adjacent_pieces point);
      helper (point + 1)
    end  
  in 
    helper cMIN_POINT_NUM 

(* populate the piece_hex hashtable. *)
let populate_piece_hex_hashtable table hex_list = 
  let f acc hex =
      Hashtbl.add table acc hex;
      acc + 1 
  in
  let _ = List.fold_left f cMIN_PIECE_NUM hex_list in ()

(* get a list of points that border 3 pieces and those pieces have higher roll probability 
  return a list of tuples of (point, evaluation stats). The list is sorted by which point have 
  higher evaluation *)
let get_first_town_options point_pieces piece_hex = 
  let f point adj_pieces out = 
    if (List.length adj_pieces) < 3 then out
    else begin 
      let helper (prob_lst, sum) ele = 
        let prob = get_roll_probablity (snd (Hashtbl.find piece_hex ele)) in
        (prob::prob_lst, prob +. sum)
      in 
        (point, List.fold_left helper ([], 0.) adj_pieces)::out
    end 
  in
  let lst = Hashtbl.fold f point_pieces [] 
  in 
  let my_compare (p2,(prob_lst2, sum2)) (p1,(prob_lst1, sum1)) = 
    if compare sum1 sum2 <> 0 then compare sum1 sum2
    else compare (List.fold_left max 0. prob_lst1) (List.fold_left max 0. prob_lst2)
  in
    List.sort my_compare lst 

(* true iff p1 is at least two roads away from any other settlement and p1 *)
(* does not have a settlement                                              *)
let is_valid_town inter_list p1 =
  let rec helper = function
    | [] -> true
    | h:: t -> is_none (List.nth inter_list h) && helper t
  in
  p1 >= cMIN_POINT_NUM && p1 <= cMAX_POINT_NUM &&
  is_none (List.nth inter_list p1) && helper (adjacent_points p1)


(* get a list of points that border a port and 2 pieces and those pieces have higher roll probability, 
  and the port has a good trade ratio.
  return a list of tuples of (point, evaluation stats). The list is sorted by which point have 
  higher evaluation *)
let get_second_town_options point_pieces piece_hex (port_list : port list) resources_from_first_town = 
  let f out ((p1, p2), ratio, kind) =
    let adj_pieces1 = adjacent_pieces p1 
    and adj_pieces2 = adjacent_pieces p2 in 
    let helper (prob_lst, sum) ele = 
      let prob = get_roll_probablity (snd (Hashtbl.find piece_hex ele)) in
      (prob::prob_lst, prob +. sum)
    in 
    let trade_points = abs(4-ratio) + (if kind = Any then 1 else 0) in
    let lst1, lst2 = 
      (List.map (fun piece -> resource_of_terrain (fst (Hashtbl.find piece_hex piece))) (adjacent_pieces p1),
       List.map (fun piece -> resource_of_terrain (fst (Hashtbl.find piece_hex piece))) (adjacent_pieces p2))
    in
    let terrain_points1, terrain_points2 = 
      (list_count (fun x -> not (List.mem x resources_from_first_town)) lst1,
       list_count (fun x -> not (List.mem x resources_from_first_town)) lst2)
    in
    let (prob_lst1, sum1) = List.fold_left helper ([], 0.) adj_pieces1  
    and (prob_lst2, sum2) = List.fold_left helper ([], 0.) adj_pieces2 in
      (p1, (prob_lst1, sum1 +. (float trade_points) +. (float terrain_points1)))::
      (p2, (prob_lst2, sum2 +. (float trade_points) +. (float terrain_points2)))::out 
  in
  let lst = List.fold_left f [] port_list
  in 
  let my_compare (p2,(prob_lst2, sum2)) (p1,(prob_lst1, sum1)) = 
    if compare sum1 sum2 <> 0 then compare sum1 sum2
    else compare (List.fold_left max 0. prob_lst1) (List.fold_left max 0. prob_lst2)
  in
    List.sort my_compare lst 

(* return a tuple (x, xs) where x is the player with color c and xs is a   *)
(* list of the rest of the player                                          *)
let get_player c player_list =
  let (x, xs) = List.partition (fun (color, _, _) -> color = c) player_list
  in
  match x with
  | [y] -> (y, xs)
  | _ -> let len = List.length x in
      failwith ( " < get_player > player with the color provided
          is not present (or present multiple times) in player_list." ^
          "player is present " ^ string_of_int len ^ " times.")

(** Returns the number of victory point associated with the type of settlement *)
let settlement_victory_point (set : settlement) : int =
  match set with
  | Town -> cVP_TOWN
  | City -> cVP_CITY

let get_num_hidden = function
  | Hidden x -> x
  | _ -> failwith "<get_num_hidden> card is a revealed card"

let get_player_vpoints player_list player inters =
  let ((_, (_, cards), (_, longestroad, largestarmy)), _) =
    get_player player player_list
  in
  let (points_from_settlement, settlements) =
    let rec helper (out, settl_lst) index = function
      | [] -> (out, settl_lst)
      | h:: t -> begin
        if is_none h || fst (get_some h) != player then helper (out, settl_lst) (index+1) t
        else helper (out + (settlement_victory_point (snd (get_some h))), index::settl_lst) (index+1)t
      end
    in
    helper (0, []) 0 inters
  in
  let points_from_trophies =
    (if longestroad then cVP_LONGEST_ROAD else 0) +
    (if largestarmy then cVP_LARGEST_ARMY else 0)
  in
  let hidden_num = get_num_hidden cards in
  let sum_points = points_from_settlement + points_from_trophies
  in
    (sum_points, hidden_num, settlements)


let get_opponents_vpoints player_list myColor inters= 
  let players = [Blue; Red; Orange; White] in
  let f acc elem = 
    if elem = myColor then acc else (elem, get_player_vpoints player_list elem inters)::acc 
  in
  let lst = List.fold_left f [] players in 
  let my_compare (_,(vp2,hidden_num2, _)) (_,(vp1,hidden_num1, _)) = 
    if compare vp1 vp2 <> 0 then compare vp1 vp2
    else compare hidden_num1 hidden_num2
  in
  let () = List.fold_left (fun () (c, (vp,h,_)) -> print_endline ("@@@@@@@(" ^ string_of_color c ^ ", " ^ string_of_int vp ^ ", " ^ string_of_int h ^ ") ")) () lst in 
    List.sort my_compare lst 

(* true iff player c has a settlement neighboring piece p *)
let has_settlement_around_piece p c inter_list =
  let f x = (not (is_none x)) && (fst (get_some x) = c) in
  list_count f (get_intersections_from_points (piece_corners p) inter_list) > 0

(* return a tuple (x, xs) where x is the player with color c and xs is a   *)
(* list of the rest of the player                                          *)
let get_player c player_list =
    let (x, xs) = List.partition (fun (color, _, _) -> color = c) player_list
    in
    match x with
    | [y] -> (y, xs)
    | _ -> let len = List.length x in
            failwith ( " < get_player > player with the color provided
                    is not present (or present multiple times) in player_list." ^
                    "player is present " ^ string_of_int len ^ " times.")
										
(* = resource1 + resource2 *)
let plus_resources resource1 resource2 = map_cost2 ( + ) resource1 resource2

(* = resource1 - resource2 *)
let subtract_resources resource1 resource2 = map_cost2 ( - ) resource1 resource2

(* returns number of settlement of type t that belong to player c  *)
let num_settlements c t inter_list =
  let f x = (not (is_none x)) && (fst (get_some x) = c) && (snd (get_some x) = t) in
  list_count f inter_list
	
let cost_fold2 : ('a -> int -> int -> 'a) -> 'a -> cost -> cost -> 'a =
    fun f v (b, w, o, l, g) (b', w', o', l', g')-> 
			f (f (f (f (f v b b') w w') o o') l l') g g'

let enough_resources : cost -> cost -> bool = 
	fun cost1 cost2 ->
		let f bool n1 n2 = bool && n1 >= n2 in cost_fold2 f true cost1 cost2
	
