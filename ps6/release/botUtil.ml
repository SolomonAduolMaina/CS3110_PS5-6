open Definition
open Constant
open Util

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
  let () = List.fold_left (fun () (p,(_,sum)) -> print_endline ("####(" ^ string_of_int p ^ ", " ^ string_of_float (sum*.36.) ^ ") ")) () lst in 

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

