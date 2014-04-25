(** Helpful functions *)

open Definition
open Constant

let _ = Random.self_init()

(******************************************************************************)
(** {2 Option utils}                                                          *)
(******************************************************************************)

(** true if None, else false *)
let is_none o = o = None

(** Unwraps Some(x) -> x, fails on None *)
let get_some = function
  Some x -> x | None -> failwith "tried to get_some of None"


(******************************************************************************)
(** {2 Random}                                                                *)
(******************************************************************************)

(** Simulate sum of rolling two dice *)
let random_roll () : roll = ((Random.int 6) + (Random.int 6) + 2)

(** Choose a random color *)
let random_color() : color = 
  match Random.int 4 with
    | 0 -> Blue
    | 1 -> Red
    | 2 -> Orange
    | 3 -> White
    | _ -> failwith "uh oh!"


(** weighted_random items weights  returns an element from items at random
 * based on the corresponding unnormalized probabilities in weights.
 * The lists must be the same length, and the sum of weights must be nonzero.
 *)
let weighted_random (items : 'a list) (weights : int list) : 'a =
  if List.length items <> List.length weights then
    failwith "weighted_random list length mismatch"
  else
    let sum = List.fold_left (+) 0 weights in
    if sum = 0 then failwith "weighted_random of all zero weights"
    else (* Find position by CDF *)
      let rand = Random.int sum in
      let choose (choice, i) x w = 
        match choice with
          | Some(c) -> (Some(c), i)
          | None ->
            let i = i + w in
            let c = if i > rand then Some(x) else None in
              (c, i)
      in
        get_some (fst (List.fold_left2 choose (None, 0) items weights))


(******************************************************************************)
(** {2 List utils}                                                            *)
(******************************************************************************)

(** Returns the number of elements in lst satisfying the predicate *)
let list_count (p : 'a -> bool) (lst : 'a list) : int =
  List.length (List.filter p lst)

(** Returns the sum of all elements in an int list *)
let list_sum : int list -> int = List.fold_left (+) 0
(** Returns the max of all elements in an int list *)
let list_max : int list -> int = List.fold_left max 0

(** Returns the index of the first element satisfying the predicate. Not [[]]-safe *)
let list_indexof (p : 'a -> bool) (lst : 'a list) : int =
  let rec index l n =
    match l with
      | [] -> failwith "indexof not found"
      | h::t -> if p h then n else index t (n+1)
  in
    index lst 0

(** Returns list with the first item satisfying the predicate removed. Not [[]]-safe *)
let rec list_memremove (p : 'a -> bool) (lst : 'a list) : 'a list =
  match lst with
    | [] -> failwith "no element to memremove"
    | h::t -> if p h then t else h::(list_memremove p t)

(** Returns Some(x) where x is an element in the list, or None for the empty list *)
let pick_random (lst : 'a list) : 'a option = 
  if lst = [] then None
  else Some (List.nth lst (Random.int (List.length lst)))

(** Returns [(x, lst - {x})], where x is a list element. Not [[]]-safe *)
let pick_one (lst : 'a list) : ('a * 'a list) =
  match lst with
  | [] -> failwith "cannot pick_one of []"
  | _ ->
    let n = Random.int (List.length lst) in
    let one = List.nth lst n in
    let split (l,num) x = 
      let l = (if num = n then l else (x::l)) in (l, num+1) 
    in
    let rem = fst (List.fold_left split ([],0) lst) in
      (one, rem)

(** Returns the list with elements in randomized order *)
let randomize (lst : 'a list) : 'a list =
  let rec iterate input output =
    match input with
      | [] -> output
      | _ -> let (next, remaining) = pick_one input in
        iterate remaining (next::output)
  in
    iterate lst []

(******************************************************************************)
(** {2 Hidden Utils}                                                          *)
(******************************************************************************)

(** Hides the cards if they are revealed *)
let hide : cards -> cards = function
  | Hidden(h) -> Hidden(h)
  | Reveal(cs) -> Hidden(List.length cs)

(** Unwrap Reveal(c) -> c *)
let reveal : cards -> card list =
  function
  | Reveal(c) -> c
  | Hidden(n) -> failwith "attempted to reveal hidden"

(** Wrap c -> Reveal(c) *)
let wrap_reveal : card list -> cards = 
  fun c -> Reveal(c)

(** Adds a card to a Reveal *)
let append_card (cs : cards) (c : card) : cards =
  wrap_reveal ((reveal cs)@[c])


(******************************************************************************)
(** {2 Cost Utils}                                                            *)
(******************************************************************************)

(** Sum of all the elements in the cost tuple *)
let sum_cost ((b,w,o,l,g) : cost) : int = b+w+o+l+g 

(** Maps a function across the cost tuple *)
let map_cost (f : int -> 'a) ((b,w,o,l,g) : cost) = (f b,f w,f o,f l,f g)

(** Maps a function across two cost tuples *)
let map_cost2 (f : int -> int -> 'a) (c1 : cost) (c2 : cost) =
  let (b1,w1,o1,l1,g1) = c1 in
  let (b2,w2,o2,l2,g2) = c2 in
    (f b1 b2, f w1 w2, f o1 o2, f l1 l2, f g1 g2)


(******************************************************************************)
(** {2 Match utils}                                                           *)
(******************************************************************************)

(** Returns Some(r) where r is the resource produced by terrain, or None if none is produced *)
let resource_of_terrain (terrain : terrain) : resource option = 
  match terrain with
    | Hill -> Some Brick 
    | Pasture -> Some Wool
    | Mountain -> Some Ore
    | Field -> Some Grain
    | Forest -> Some Lumber
    | Desert -> None

(** Returns a cost where there is one of the resource specified, and zero of all others *)
let single_resource_cost (resource : resource) : cost = 
  match resource with
    | Brick ->  (1,0,0,0,0)
    | Wool ->   (0,1,0,0,0)
    | Ore ->    (0,0,1,0,0)
    | Grain ->  (0,0,0,1,0)
    | Lumber -> (0,0,0,0,1)

(** Returns the number of resources generated by a type of settlement *)
let settlement_num_resources (set : settlement) : int =
  match set with
    | Town -> cRESOURCES_GENERATED_TOWN
    | City -> cRESOURCES_GENERATED_CITY

(** Returns the number of a specific resource in an inventory *)
let num_resource_in_inventory (inv : inventory) (res : resource) : int =
  let (b,w,o,l,g) = inv in
    match res with
      | Brick -> b
      | Wool -> w
      | Ore -> o
      | Grain -> g
      | Lumber -> l

(** Returns the cost of building a build *)
let cost_of_build (build: build) : cost = 
  match build with
    | BuildRoad(_) -> cCOST_ROAD
    | BuildTown(_) -> cCOST_TOWN
    | BuildCity(_) -> cCOST_CITY
    | BuildCard -> cCOST_CARD

(** Returns the next turn's color in normal forward order *)
let next_turn (color : color) : color =
  match color with
    | Blue -> Red
    | Red -> Orange
    | Orange -> White
    | White -> Blue

(** Returns the next turn's color in reverse order *)
let prev_turn (color : color) : color =
  match color with
    | Blue -> White
    | Red -> Blue
    | Orange -> Red
    | White -> Orange

(** Identifies the card variant based on the playcard *)
let card_of_playcard (play : playcard) : card =
  match play with
    | PlayKnight(_) -> Knight
    | PlayRoadBuilding(_) -> RoadBuilding
    | PlayYearOfPlenty(_) -> YearOfPlenty
    | PlayMonopoly(_) -> Monopoly

(** Returns a list of points reachable from point in one road length *)
let adjacent_points (point : point) : point list = 
  match point with
    | 0 -> [1;8] | 1 -> [0;2] | 2 -> [1;3;10] | 3 -> [2;4] | 4 -> [3;5;12] | 5 -> [4;6] | 6 -> [5;14]
    | 7 -> [8;17] | 8 -> [0;7;9] | 9 -> [8;10;19] | 10 -> [2;9;11] | 11 -> [10;12;21] | 12 -> [4;11;13] | 13 -> [12;14;23]  | 14 -> [6;13;15] | 15 -> [14;25]
    | 16 -> [17;27] | 17 -> [7;16;18] | 18 -> [17;19;29] | 19 -> [9;18;20] | 20 -> [19;21;31] | 21 -> [11;20;22] | 22 -> [21;23;33] | 23 -> [13;22;24] | 24 -> [23;25;35] | 25 -> [15;24;26]
    | 26 -> [25;37] | 27 -> [16;28] | 28 -> [27;29;38] | 29 -> [18;28;30] | 30 -> [29;31;40] | 31 -> [20;30;32] | 32 -> [31;33;42] | 33 -> [22;32;34] | 34 -> [33;35;44] | 35 -> [24;34;36] | 36 -> [35;37;46] | 37 -> [26;36]
    | 38 -> [28;39] | 39 -> [38;40;47] | 40 -> [30;39;41] | 41 -> [40;42;49] | 42 -> [32;41;43] | 43 -> [42;44;51] | 44 -> [34;43;45] | 45 -> [44;46;53] | 46 -> [36;45]
    | 47 -> [39;48] | 48 -> [47;49] | 49 -> [41;48;50] | 50 -> [49;51] | 51 -> [43;50;52] | 52 -> [51;53] | 53 -> [45;52]
    | _ -> failwith "invalid point"

(** Returss a list of hex indices corresponding to pieces that point borders *)
let adjacent_pieces (point : point) : piece list =
  match point with
    | 0 -> [0] | 1 -> [0] | 2 -> [0;1] | 3 -> [1] | 4 -> [1;2] | 5 -> [2] | 6 -> [2] 
    | 7 -> [3] | 8 -> [0;3] | 9 -> [0;3;4] | 10 -> [0;1;4] | 11 -> [1;4;5] | 12 -> [1;2;5] | 13 -> [2;5;6] | 14 -> [2;6] | 15 -> [6] 
    | 16 -> [7] | 17 -> [3;7] | 18 -> [3;7;8] | 19 -> [3;4;8] | 20 -> [4;8;9] | 21 -> [4;5;9] | 22 -> [5;9;10] | 23 -> [5;6;10] | 24 -> [6;10;11] | 25 -> [6;11] | 26 -> [11] 
    | 27 -> [7] | 28 -> [7;12] | 29 -> [7;8;12] | 30 -> [8;12;13] | 31 -> [8;9;13] | 32 -> [9;13;14] | 33 -> [9;10;14] | 34 -> [10;14;15] | 35 -> [10;11;15] | 36 -> [11;15] | 37 -> [11] 
    | 38 -> [12] | 39 -> [12;16] | 40 -> [12;13;16] | 41 -> [13;16;17] | 42 -> [13;14;17] | 43 -> [14;17;18] | 44 -> [14;15;18] | 45 -> [15;18] | 46 -> [15] 
    | 47 -> [16] | 48 -> [16] | 49 -> [16;17] | 50 -> [17] | 51 -> [17;18] | 52 -> [18] | 53 -> [18] 
    | _ -> failwith "invalid point"

(** Returns the indices of all points bordering the hex piece *)
let piece_corners (piece : piece) : point list =
  match piece with
                        | 0 -> [0;1;2;8;9;10] | 1 -> [2;3;4;10;11;12] | 2 -> [4;5;6;12;13;14]
            | 3 -> [7;8;9;17;18;19] | 4 -> [9;10;11;19;20;21] | 5 -> [11;12;13;21;22;23] | 6 -> [13;14;15;23;24;25]
| 7 -> [16;17;18;27;28;29] | 8 -> [18;19;20;29;30;31] | 9 -> [20;21;22;31;32;33] | 10 -> [22;23;24;33;34;35] | 11 -> [24;25;26;35;36;37]
            | 12 -> [28;29;30;38;39;40] | 13 -> [30;31;32;40;41;42] | 14 -> [32;33;34;42;43;44] | 15 -> [34;35;36;44;45;46]
                | 16 -> [39;40;41;47;48;49] | 17 -> [41;42;43;49;50;51] | 18 -> [43;44;45;51;52;53]
    | _ -> failwith "invalid piece number"



(******************************************************************************)
(** {2 Longest road}                                                          *)
(******************************************************************************)

(** Returns the road-length of a player's longest road *)
let longest_road (c : color) (roads : road list) (inters : intersection list) : int =
  let no_enemy (p : point) : bool =
    match List.nth inters p with
      | None -> true
      | Some(color, _) -> c = color
  in
  (* sets are split by enemy settlements if there are no other connections *)
  let includes (lst : line list) ((a,b) : line) : bool = 
    let includes_one (c,d) = 
      ((a = c || b = c) && no_enemy c) || ((a = d || b = d) && no_enemy d)
    in
      List.exists includes_one lst
  in
  (* partition the set of lines into connected components *)
  let split_sets (lines : line list) : line list list =
    (* returns the partition of completed set * remaining lines *)
    let rec one_set (start : line list) (rest : line list) : (line list * line list) =
      let (added,r) = List.partition (includes start) rest in
      match added with
        | [] -> start, rest
        | added -> one_set (added@start) r
    in
    (* completely partition a set of lines *)
    let rec build_sets (sets : line list list) (lines_remaining : line list) : line list list = 
      match lines_remaining with
        | [] -> sets
        | h::t -> (
          let (new_set, remaining) = one_set [h] t in
            build_sets (new_set::sets) remaining)
    in
      build_sets [] lines
  in
  (* traverse the set from each endpoint *)
  let count_length (set : line list) : int =
    (* return lst with all duplicates removed *)
    let rec remove_duplicates lst =
      match lst with
        | h::t -> let rest = remove_duplicates t in
          if List.mem h t then rest else h::rest
        | [] -> []
    in
    (* list of all the points in the set of lines *)
    let all_points set =
      remove_duplicates (List.flatten (List.map (fun (a,b) -> [a;b]) set))
    in
    (* return points in the set that are connected to p *)
    let connected_points p =
      let rec find lst points =
        match lst with
          | (a, b)::t ->
            if a = p then find t (b::points)
            else if b = p then find t (a::points)
            else find t points
          | [] -> points
      in
        find set []
    in
    let not_including lst i = List.filter ((<>) i) lst in
    (* find the maximum length traversing from start *)
    let traverse points start =
      (* count the points traversed *)
      let rec traverse_count ps s =
        match not_including ps s with
          | [] -> 1
          | rest ->
            let connected = connected_points s in
            let paths = List.filter (fun x -> List.mem x rest) connected in
            let lengths = List.map (traverse_count rest) paths in
              (list_max lengths) + 1
      in
        (* length is 1 less than points traversed *)
        (traverse_count points start) - 1
    in
    (* Find the max length starting from each point *)
    let setpoints = all_points set in
    let lengths = List.map (traverse setpoints) setpoints in
        list_max lengths
  in
  (* Maximum of all of the player's sets *)
  let my_lines = List.map snd roads in
  let sets = split_sets my_lines in
    list_max (List.map count_length sets)

   
(******************************************************************************)
(** {2 State generation}                                                      *)
(******************************************************************************)

(** Returns a blank turn with active = color *)
let new_turn (c : color) : turn = {active = c;
                                   dicerolled = None;
                                   cardplayed = false;
                                   cardsbought = Reveal([]);
                                   tradesmade = 0;
                                   pendingtrade = None}


(** Default initial state generator *)
let gen_initial_state () : state = 
  let c = random_color() in
    (cDEFAULT_BOARD, cDEFAULT_PLAYERS, new_turn c, (c, InitialRequest))


(** Produces random initial states for experimenting with bots *)
let gen_random_initial_state () : state = 
  let gen_random_map () : map =
    let (terrains, rolls) = List.split cDEFAULT_HEXES in
    let hexes = List.combine (randomize terrains) (randomize rolls) in
      (hexes, cDEFAULT_PORTS)
  in
  let gen_random_board () : board =
    let (hexes, _) as map = gen_random_map() in
    let robber = list_indexof (fun (t,_) -> t = Desert) hexes in
      (map, cDEFAULT_STRUCTURES, cDEFAULT_DECK, cDEFAULT_DISCARD, robber)
  in
  let c = random_color() in
    (gen_random_board(), cDEFAULT_PLAYERS, new_turn c, (c, InitialRequest))
