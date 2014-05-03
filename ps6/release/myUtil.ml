open Definition
open Constant
open Util
open Print

let _ = Random.self_init()

(*******************************************************)
(*********  string representation of tupes *************)
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

let random_resource () : resource =
	match Random.int 5 with
	| 0 -> Brick
	| 1 -> Wool
	| 2 -> Ore
	| 3 -> Grain
	| _ -> Lumber

let random_cost () : cost =
	let n1 = Random.int 6 in
	let n2 = Random.int 6 in
	let n3 = Random.int 6 in
	let n4 = Random.int 6 in let n5 = Random.int 6 in (n1, n2, n3, n4, n5)

(* return a tuple (x, xs) where x is the player with color c and xs is a   *)
(* list of the rest of the player                                          *)
let get_player c player_list =
	let (x, xs) = List.partition (fun (color, _, _) -> color = c) player_list
	in
	match x with
	| [y] -> (y, xs)
	| _ -> failwith " < get_player > player with the color provided
is not present (or present multiple times) in player_list"

(* set element at index i to e. preserves order of list *)
let list_update_elem (i : int) (e : 'a) (lst : 'a list) : 'a list =
	let rec helper i l1 current l2 =
		if i = 0 then List.rev_append l1 (e:: l2)
		else match l2 with
			| [] -> failwith "<list_update_elem> index is out of range"
			| h:: t -> helper (i -1) (current:: l1) h t
	in
	match lst with
	| [] -> failwith "<list_update_elem> lst is empty"
	| h:: t -> helper i [] h t

(* update [inter_list] to add settlement for player [color] at [point] of  *)
(* type [settl_type]                                                       *)
let add_settlement point color settl_type inter_list =
	list_update_elem point (Some (color, settl_type)) inter_list

(* returns number of towns on the board *)
let num_towns inter_list =
	let f x = (not (is_none x)) && (snd (get_some x) = Town) in
	list_count f inter_list

(* returns number of cities on the board *)
let num_cities inter_list =
	let f x = (not (is_none x)) && (snd (get_some x) = City) in
	list_count f inter_list

(* true iff a line exist between p1 and p2 *)
let is_valid_line (p1, p2) = List.length (List.filter (fun x -> x = p2) (adjacent_points p1)) = 1

(* true iff p1 is at least two roads away from any other settlement and p1 *)
(* does not have a settlement                                              *)
let is_valid_town inter_list p1 =
	let rec helper = function
		| [] -> true
		| h:: t -> is_none (List.nth inter_list h) && helper t
	in
	is_none (List.nth inter_list p1) && helper (adjacent_points p1)

(* = resource1 + resource2 *)
let plus_resources resource1 resource2 = map_cost2 ( + ) resource1 resource2

(* = resource1 - resource2 *)
let subtract_resources resource1 resource2 = map_cost2 ( - ) resource1 resource2

(* if resource = (r1, r2, r3, r4, r5), return (num *r1, num *r2, num *r3,  *)
(* num *r4, num *r5)                                                       *)
let mult_resources resource num = map_cost (fun x -> num * x) resource

(* return the hexes corresponding to the a pieces in plst. In output, hex  *)
(* are ordered by increasing based on their piece number                   *)
let get_hexes_from_pieces (plst : piece list) (hlst : hex list): hex list =
	let rec helper out plst hlst nth =
		match plst, hlst, nth with
		| [], _, _ -> List.rev out
		| ph:: pt, hh:: ht, n -> begin
					if ph = n then helper (hh:: out) pt ht (n +1)
					else helper out plst ht (n +1)
				end
		| _ -> failwith "<get_hexes_from_pieces> entries in plst don't map to entries in hlst"
	in
	helper [] (List.sort (compare) plst) hlst 0

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

(* gen num amount of resources from given hex or zero resources if tar =   *)
(* Desert                                                                  *)
let gen_hex_resource (num : int) ((tar, _) : hex) =
	let res = resource_of_terrain tar in
	if is_none res then (0,0,0,0,0)
	else map_cost (fun x -> x * num ) (single_resource_cost (get_some res))

(* return the sum of num amount of resources generated by each hex that's  *)
(* neighbors p and not occupied by robber                                  *)
let gen_all_resources p num hex_list robber =
	let adj = List.filter (fun x -> x <> robber) (adjacent_pieces p) in
	let hexes = get_hexes_from_pieces adj hex_list in
	let f acc elem = plus_resources acc (gen_hex_resource num elem) in
	List.fold_left f (0,0,0,0,0) hexes

(* add resources r to the inventory of player c *)
let add_resources_to_player c r player_list =
	let f ((color, (inventory, cards), trophies) as pl) =
		if color = c then ((color, (plus_resources inventory r, cards), trophies)) else pl
	in
	List.map f player_list

(* subtract resources r to the inventory of player c *)
let subtract_resources_from_player c r player_list =
	let f ((color, (inventory, cards), trophies) as pl) =
		if color = c then ((color, (subtract_resources inventory r, cards), trophies)) else pl
	in
	List.map f player_list

let random_resource_type () = match Random.int 5 with
	| 0 -> Brick | 1 -> Wool | 2 -> Ore | 3 -> Grain | _ -> Lumber

(* return a random single resource cost of a resource from inventory. if   *)
(* inventory is empty, return (0,0,0,0,0)                                  *)
let get_a_resource_from inventory =
	if sum_cost inventory = 0 then (0,0,0,0,0)
	else begin
		let rec helper () =
			let res = random_resource_type () in
			if num_resource_in_inventory inventory res > 0 then single_resource_cost res
			else helper ()
		in
		helper ()
	end

(* return a random single resource cost of a resource that this player     *)
(* has. If player's inventory is empty return (0,0,0,0,0)                  *)
let get_single_avaliable_resource (color, (inventory, cards), trophies) =
	get_a_resource_from inventory

(* steal a resource from player c1 and give it to player c2. If c1 has     *)
(* empty inventory then pl is returned unchanged                           *)
let steal_from_and_give_to c1 c2 pl =
	let rec steal l1 l2 =
		match l2 with
		| [] -> failwith "This surely cannot happen"
		| ((color, (inv, cards), ts) as p):: t ->
				match color <> c1 with
				| true -> steal (p:: l1) t
				| false ->
						let stolen = get_single_avaliable_resource p in
						let p = (color, (subtract_resources inv stolen, cards), ts)
						in ((p:: l1) @ t, stolen)
	in
	let rec give l1 l2 stolen =
		match l2 with
		| [] -> failwith "This surely cannot happen"
		| ((color, (inv, cards), ts) as p):: t ->
				match color <> c2 with
				| true -> give (p:: l1) t stolen
				| false ->
						let p = (color, (plus_resources inv stolen, cards), ts)
						in (p:: l1) @ t
	in let (ls, stolen) = steal [] pl in give [] ls stolen

(* true iff player c has a settlement neighboring piece p *)
let has_settlement_around_piece p c inter_list =
	let f x = (not (is_none x)) && (fst (get_some x) = c) in
	list_count f (get_intersections_from_points (piece_corners p) inter_list) > 0

(* computes (total inventory)/2 - (total cost). *)
let is_floor_half (color, (inventory, cards), trophies) cost =
	sum_cost (inventory) /2 - sum_cost(cost)

(* true iff player's inventory is greater than the max hand size a player  *)
(* can have before needing to discard when the robber is activated         *)
let needs_to_discard (color, (inventory, cards), trophies) =
	sum_cost inventory > cMAX_HAND_SIZE

(* true iff the player has enough resources in their inventory to cover    *)
(* cost                                                                    *)
let has_enough_resources (color, (inventory, cards), trophies) cost =
	let (d1, d2, d3, d4, d5) = subtract_resources inventory cost in
	d1 >=0 && d2 >=0 && d3 >=0 && d4 >=0 && d5 >=0

(* check if inventory can cover cost, and that cost is floor half of       *)
(* inventory return new olayer with update inventory, and also return the  *)
(* modified cost. if cost was not modified then return cost                *)
let check_and_fix_discard_move ((color, (inventory, cards), trophies) as pl) cost : player * cost =
	let new_inventory, new_cost =
		if has_enough_resources pl cost && (is_floor_half pl cost = 0)
		then (subtract_resources inventory cost, cost)
		else begin
			let rec helper remain cost =
				if is_floor_half pl cost = 0 then (remain, cost)
				else
					let single = get_a_resource_from remain in
					helper (subtract_resources remain single) (plus_resources cost single)
			in
			let half = map_cost (fun x -> x /2) inventory in
			helper (subtract_resources inventory half) (half)
		end
	in
	((color, (new_inventory, cards), trophies), new_cost)

(* return a list of tuples (p, h) such that h is a hex associated with     *)
(* roll and p is its piec number                                           *)
let get_hex_with_roll hex_list roll : (int * hex) list =
	let rec helper index out = function
		| [] -> out
		| ((_, roll') as h):: t -> begin
					if roll = roll' then helper (index +1) ((index, h):: out) t
					else helper (index +1) out t
				end
	in
	helper 0 [] hex_list

(* generate resources for every settlement that borders a hex              *)
(* corresponding to the dice roll if not inhabited by the robber. Generate *)
(* resources are added to the owner of the settlement                      *)
let gen_roll_resources pl inter_list hex_list roll robber =
	let h = get_hex_with_roll hex_list roll
	in
	(* let () = List.fold_left (fun () (p, h) -> print_endline (" hex at ("  *)
	(* ^ (string_of_int p) ^ ", " ^ string_of_hex h)) () h in                *)
	let (_, hexes) =
		List.partition (fun (p, _) -> p = robber) h
	in
	(* let () = List.fold_left (fun () (p, h) -> print_endline (" hexes at   *)
	(* (" ^ (string_of_int p) ^ ", " ^ string_of_hex h)) () hexes in         *)
	let piece_resource =
		let f (p, (tar, _)) = (p, if is_none (resource_of_terrain tar) then (0,0,0,0,0)
				else single_resource_cost (get_some (resource_of_terrain tar)))
		in
		List.map f hexes
	in
	(* let () = List.fold_left (fun () (p, h) -> print_endline ("            *)
	(* piece_resource at (" ^ (string_of_int p) ^ ", " ^                     *)
	(* (string_of_resources h))) () piece_resource in                        *)
	let points_resources =
		let f (p, resource) = List.map (fun p -> (p, resource)) (piece_corners p) in
		List.flatten (List.map f piece_resource)
	in
	(* let () = List.fold_left (fun () (p, h) -> print_endline ("            *)
	(* point_resources at (" ^ (string_of_int p) ^ ", " ^                    *)
	(* (string_of_resources h))) () points_resources in                      *)
	let ordered_points_resources =
		List.sort (fun (x1, _) (x2, _) -> compare x2 x1) points_resources
	in
	(* let () = List.fold_left (fun () (p, h) -> print_endline ("            *)
	(* ordered_points_resources at (" ^ (string_of_int p) ^ ", " ^           *)
	(* (string_of_resources h))) () ordered_points_resources in              *)
	let combined =
		let rec helper out (p, sum) = function
			| [] -> (p, sum):: out
			| (p1, r1):: t -> begin
						if p1 = p then helper out (p, plus_resources r1 sum) t
						else helper ((p, sum):: out) (p1, r1) t
					end
		in
		match ordered_points_resources with
		| [] -> []
		| h:: t -> helper [] h t
	in
	(* let () = List.fold_left (fun () (p, h) -> print_endline (" combined   *)
	(* at (" ^ (string_of_int p) ^ ", " ^ (string_of_resources h))) ()       *)
	(* combined in                                                           *)
	let (points, sum_res) = List.split combined
	in
	let inters_resources =
		List.combine (get_intersections_from_points points inter_list) sum_res
	in
	(* let () = List.fold_left (fun () (p, h) -> print_endline ("            *)
	(* inters_resources at (" ^ (string_of_intersection p) ^ ", " ^          *)
	(* (string_of_resources h))) () inters_resources in                      *)
	let settl_resources =
		let f (inter, x) = if is_none inter then [] else [(get_some inter, x)] in
		List.flatten (List.map f inters_resources)
	in
	(* let () = List.fold_left (fun () ((c,s), h) -> print_endline ("        *)
	(* settl_resources at (" ^ "[" ^ (string_of_color c) ^ "; " ^            *)
	(* (string_of_settlement s) ^ "], " ^ (string_of_resources h))) ()       *)
	(* settl_resources in                                                    *)
	let rec add_resources working_pl = function
		| [] -> working_pl
		| ((c, settl_type), sum):: t -> begin
					let wieghted_sum = mult_resources sum (settlement_num_resources settl_type)
					in
					add_resources (add_resources_to_player c wieghted_sum working_pl) t
				end
	in
	add_resources pl settl_resources

let road_not_bought : road -> road list -> bool =
	fun (_, (p1, p2)) roads ->
			let f (_, (p3, p4)) = ((p1, p2) <> (p3, p4)) && ((p1, p2) <> (p4, p3)) in
			List.for_all f roads

let valid_road_build : road -> road list -> bool =
	fun ((c1, (p1, p2)) as road) roads ->
			match is_valid_line (p1, p2) && road_not_bought road roads with
			| true ->
					let f bool (c2, (p3, p4)) =
						let adjacent = p1 = p3 || p1 = p4 || p2 = p3 || p2 = p4 in
						let same_player = c1 = c2 in
						let valid = adjacent && same_player in
						bool || valid in List.fold_left f false roads
			| false -> false

let touches_player_road : color -> point -> road list -> bool =
	fun colour p roads ->
			let f bool (c, (p1, p2)) = bool || (c = colour && (p = p1 || p = p2)) in
			List.fold_left f false roads

let valid_town_build : color -> point -> structures -> bool =
	fun colour p structs ->
			let (insecs, roads) = structs in
			is_valid_town insecs p && touches_player_road colour p roads

let player_settlements_built : color -> settlement -> intersection list -> int =
	fun c setl inter_list ->
			let f x =
				(not (is_none x)) && (let (a, b) = get_some x in (a = c) && (b = setl))
			in list_count f inter_list

let player_roads_built : color -> road list -> int =
	fun c roads ->
			let f n (colour, _) = if colour = c then n + 1 else n
			in List.fold_left f 0 roads

let cost_fold : ('a -> int -> 'a) -> 'a -> cost -> 'a =
	fun f v (b, w, o, l, g) -> f (f (f (f (f v b) w) o) l) g

let least_ratio : color -> port list -> intersection list -> resource -> ratio =
	fun c ports l resource ->
			let f n ((a, b), r, res) =
				let res = match res with
					| Any -> resource
					| PortResource r' -> r' in
				let p1, p2 = (List.nth l a), (List.nth l b) in
				match not (is_none p1) , not (is_none p2) with
				| true, true -> let (c1, _), (c2, _) = get_some p1 , get_some p2 in
						if (c1 = c || c2 = c) && r < n && resource = res then r else n
				| true, false -> let (c1, _) = get_some p1 in
						if c1 = c && r < n && resource = res then r else n
				| false, true -> let (c2, _) = get_some p2 in
						if c2 = c && r < n && resource = res then r else n
				| false, false -> n
			in List.fold_left f cMARITIME_DEFAULT_RATIO ports
