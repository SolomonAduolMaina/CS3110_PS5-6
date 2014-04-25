(** Convenience Functions for printing -- You only need to call print_update. *)


open Definition
open Constant
open Util
open Printf


(******************************************************************************)
(** {2 Troubleshooting}                                                       *)
(******************************************************************************)

(** If you want to print anything based on the state,
    Add it here, and it will be printed beneath the board at each update *)
let print_test (s : state) : string = ""


(* 2 String Conversion                                                       *)

let print_ref = ref true
let toggle_print b = print_ref := b
let to_print () = !print_ref

let names_ref = ref ("Blue", "Red", "Orange", "White")
let set_names names = names_ref := names

let print = print_endline
let soi = string_of_int

let name_of_color c =
  let (b,r,o,w) = !names_ref in
  match c with
   | Blue -> b
   | Red -> r
   | Orange -> o
   | White -> w

let string_of_color = function
  | Blue -> "Blue"
  | Red -> "Red"
  | Orange -> "Orange"
  | White -> "White"

let string_of_terrain = function
  | Hill -> "Hill"
  | Pasture -> "Pasture"
  | Mountain -> "Mountain"
  | Field -> "Field"
  | Forest -> "Forest"
  | Desert -> "Desert"

let string_of_resource = function
  | Brick -> "Brick"
  | Wool -> "Wool"
  | Ore -> "Ore"
  | Grain -> "Grain"
  | Lumber -> "Lumber"
  
let string_of_resource_dot = function
  | Brick -> "B"
  | Wool -> "W"
  | Ore -> "O"
  | Grain -> "G"
  | Lumber -> "L"
  
let string_of_card = function
  | Knight -> "K"
  | VictoryPoint -> "VP"
  | RoadBuilding -> "RB"
  | YearOfPlenty -> "YoP"
  | Monopoly -> "M"

let string_of_card_full = function
  | Knight -> "Knight"
  | VictoryPoint -> "VictoryPoint"
  | RoadBuilding -> "RoadBuilding"
  | YearOfPlenty -> "YearOfPlenty"
  | Monopoly -> "Monopoly"

let string_of_settlement = function
  | Town -> "Town"
  | City -> "City"

let string_of_request = function
  | InitialRequest -> "InitialRequest"
  | RobberRequest -> "RobberRequest"
  | DiscardRequest -> "DiscardRequest"
  | TradeRequest -> "TradeRequest"
  | ActionRequest -> "ActionRequest"

let string_of_list (to_string : 'a -> string) (lst : 'a list) =
  let lst = List.filter (fun x -> to_string x <> "") lst in
    match lst with
      | [] -> ""
      | _ -> "[" ^ (String.concat ", " (List.map to_string lst)) ^ "]"

let string_of_cost cost = 
  let (b,w,o,g,l) = map_cost soi cost in
  let lst = [(b, " B"); (w, " W"); (o, " O"); (g, " G"); (l, " L")] in
  let lst = List.filter (fun (i, _) -> i <> "0") lst in
    string_of_list (fun (i, r) -> i ^ r) lst

let string_of_cards : card list -> string =
  string_of_list string_of_card

let string_of_trophies ((n, lr, la) : trophies) : string =
  let lst = [if n > 0 then (soi n) ^ " K" else "";
             if lr then "LR" else "";
             if la then "LA" else ""] in
    string_of_list (fun s -> s) lst



(*------------------------------ ANSI codes ----------------------------------*)

let back = "\b \b"

let clear = "\027[2J\027[1;1H"

let blue = "\027[38;5;33m"
let red = "\027[38;5;124m"
let white = "\027[38;5;15m"
let orange = "\027[38;5;208m"
let green = "\027[38;5;34m"
let background = "\027[38;5;237m"

let code_of_color = function
  | Blue -> blue
  | Red -> red
  | White -> white
  | Orange -> orange


(*------------------------------ Printing ------------------------------------*)

let print_board (b : board) : string =
  let (map, structures, deck, discard, robber) = b in
  let (hexes, ports) = map in
  let (intersections, roads) = structures in
  (* board 1 *)
  let nl = "\t\t\t\t" ^ background in
  let h = List.nth hexes in
  let n x =
    let r = (snd (h x)) in
    let num = if r = 0 then "- " else if r < 10 then (soi r) ^ " " else soi r in
    let clr = if r = 6 || r = 8 then red else green in
      clr ^ num ^ background
  in
  let o x = 
    let find_port a ((p1, p2), _, r) =
        let f = 
          if x = p1 || x = p2 then 
	          (match r with 
	            | PortResource(res) -> string_of_resource_dot res
	            | Any -> "*")
          else a
        in
          green ^ f ^ background
    in
      List.fold_left find_port " " ports
  in
  (* board 2 *)
  let p x : string = 
    (match List.nth intersections x with
      | None -> " "
      | Some(c, Town) -> (code_of_color c) ^ "o"
      | Some(c, City) -> (code_of_color c) ^ "@") ^ background
  in
  let t x : string =
    green ^ (match resource_of_terrain (fst (List.nth hexes x)) with
      | None -> "_"
      | Some(r) -> string_of_resource_dot r)
    ^ (if robber = x then "#" else " ") ^ background
  in
  let r st p1 p2 : string =
    (let rec find_road rds =
      match rds with
        | [] -> None
        | (c, (r1,r2))::t -> 
          if (r1,r2) = (p1,p2) || (r2,r1) = (p1,p2) 
          then Some c else find_road t
    in
      (match find_road roads with
        | None -> st
        | Some(c) ->(code_of_color c) ^ st)) ^ background
  in
  let b = r "\\" and f = r "/" and u = r "___" in
  let format = format_of_string (* #unclear *)
    "
%s                 _______                                       _______
%s                /%s_____%s\\                                     / %s%s%s \\
%s          _____/ /     \\ \\_____                         _____/ %s     %s \\_____
%s         /%s____%s/   %s  \\%s____%s\\                       / %s%s%s%s   %s  %s%s%s%s \\
%s   _____/ /     \\       /     \\ \\_____           _____/ %s     %s       %s     %s \\_____
%s  /%s____%s/   %s  \\_____/   %s  \\%s____%s\\         / %s%s%s%s   %s  %s%s%s%s%s   %s  %s%s%s%s \\
%s / /     \\       /     \\       /     \\ \\       / %s     %s       %s     %s       %s     %s \\
%s/%s/   %s  \\_____/   %s  \\_____/   %s  \\%s\\     /%s%s   %s  %s%s%s%s%s   %s  %s%s%s%s%s   %s  %s%s\\
%s\\ \\       /     \\       /     \\       / /     \\ %s       %s     %s       %s     %s       %s /
%s \\%s\\_____/   %s  \\_____/   %s  \\_____/%s/       \\ %s%s%s%s%s   %s  %s%s%s%s%s   %s  %s%s%s%s%s /
%s / /     \\       /     \\       /     \\ \\       / %s     %s       %s     %s       %s     %s \\
%s/%s/   %s  \\_____/   %s  \\_____/   %s  \\%s\\     /%s%s   %s  %s%s%s%s%s   %s  %s%s%s%s%s   %s  %s%s\\
%s\\ \\       /     \\       /     \\       / /     \\ %s       %s     %s       %s     %s       %s /
%s \\%s\\_____/   %s  \\_____/   %s  \\_____/%s/       \\ %s%s%s%s%s   %s  %s%s%s%s%s   %s  %s%s%s%s%s /
%s / /     \\       /     \\       /     \\ \\       / %s     %s       %s     %s       %s     %s \\
%s/%s/   %s  \\_____/   %s  \\_____/   %s  \\%s\\     /%s%s   %s  %s%s%s%s%s   %s  %s%s%s%s%s   %s  %s%s\\
%s\\ \\       /     \\       /     \\       / /     \\ %s       %s     %s       %s     %s       %s /
%s \\ \\_____/   %s  \\_____/   %s  \\_____/ /       \\ %s%s%s%s%s   %s  %s%s%s%s%s   %s  %s%s%s%s%s /
%s  \\%s____%s\\       /     \\       /%s____%s/         \\_____ %s       %s     %s       %s _____/
%s        \\ \\_____/   %s  \\_____/ /                     \\ %s%s%s%s%s   %s  %s%s%s%s%s /
%s         \\%s____%s\\       /%s____%s/                       \\_____ %s       %s _____/
%s               \\ \\_____/ /                                   \\ %s%s%s%s%s /
%s                \\%s_____%s/                                     \\_______/
\n"
    in
    sprintf format nl (* linewise b1/b2 args *) 
      nl (o 27) (o 16)
         (p 27) (u 27 16) (p 16)
      nl
         (f 27 28) (b 16 17)
      nl (o 38) (o 28) (n 7) (o 17) (o 7)
         (p 38) (u 38 28) (p 28) (f 27 28) (t 7) (b 16 17) (p 17) (u 17 7) (p 7) 
      nl
         (f 38 39) (b 28 29) (f 18 17) (b 7 8)
      nl (o 47) (o 39) (n 12) (n 3) (o 8) (o 0)
         (p 47) (u 47 39) (p 39) (f 38 39) (t 12) (b 28 29) (p 29) (u 29 18) (p 18) (f 18 17) (t 3) (b 7 8) (p 8) (u 8 0) (p 0)
      nl
         (f 47 48) (b 39 40) (f 29 30) (b 18 19) (f 8 9) (b 0 1)
      nl (o 48) (n 16) (n 8) (n 0) (o 1)
         (p 48) (f 47 48) (t 16) (b 39 40) (p 40) (u 40 30) (p 30) (f 29 30) (t 8) (b 18 19) (p 19) (u 19 9) (p 9) (f 8 9) (t 0) (b 0 1) (p 1)
      nl
         (b 48 49) (f 40 41) (b 30 31) (f 19 20) (b 9 10) (f 1 2)
      nl (o 49) (n 13) (n 4) (o 2)
         (b 48 49) (p 49) (u 49 41) (p 41) (f 40 41) (t 13) (b 30 31) (p 31) (u 31 20) (p 20) (f 19 20) (t 4) (b 9 10) (p 10) (u 10 2) (p 2) (f 1 2)
      nl
         (f 49 50) (b 41 42) (f 31 32) (b 20 21) (f 10 11) (b 2 3)
      nl (o 50) (n 17) (n 9) (n 1) (o 3)
         (p 50) (f 49 50) (t 17) (b 41 42) (p 42) (u 42 32) (p 32) (f 31 32) (t 9) (b 20 21) (p 21) (u 21 11) (p 11) (f 10 11) (t 1) (b 2 3) (p 3)
      nl
         (b 50 51) (f 42 43) (b 32 33) (f 21 22) (b 11 12) (f 3 4)
      nl (o 51) (n 14) (n 5) (o 4)
         (b 50 51) (p 51) (u 51 43) (p 43) (f 42 43) (t 14) (b 32 33) (p 33) (u 33 22) (p 22) (f 21 22) (t 5) (b 11 12) (p 12) (u 12 4) (p 4) (f 3 4)
      nl
         (f 51 52) (b 43 44) (f 33 34) (b 22 23) (f 12 13) (b 4 5)
      nl (o 52) (n 18) (n 10) (n 2) (o 5)
         (p 52) (f 51 52) (t 18) (b 43 44) (p 44) (u 44 34) (p 34) (f 33 34) (t 10) (b 22 23) (p 23) (u 23 13) (p 13) (f 12 13) (t 2) (b 4 5) (p 5)
      nl
         (b 52 53) (f 44 45) (b 34 35) (f 23 24) (b 13 14) (f 5 6)
      nl (n 15) (n 6)
         (b 52 53) (p 53) (u 53 45) (p 45) (f 44 45) (t 15) (b 34 35) (p 35) (u 35 24) (p 24) (f 23 24) (t 6) (b 13 14) (p 14) (u 14 6) (p 6) (f 5 6)
      nl (o 53) (o 45) (o 14) (o 6)
         (b 45 46) (f 35 36) (b 24 25) (f 14 15)
      nl (n 11)
         (b 45 46) (p 46) (u 46 36) (p 36) (f 35 36) (t 11) (b 24 25) (p 25) (u 25 15) (p 15) (f 14 15)
      nl (o 46) (o 36) (o 25) (o 15)
         (b 36 37) (f 25 26)
      nl
         (b 36 37) (p 37) (u 37 26) (p 26) (f 25 26)
      nl (o 37) (o 26)


(* Would break for num_players <> 4 *)
let print_players (players : player list) : string = 
  let strings_of_players (p : player) : string * string * string * string =
    let (color, (inventory, cards), trophies) = p in
    let cards = match cards with
      | Hidden(n) -> []
      | Reveal(lst) -> lst
    in
    let c = code_of_color color in
    let s1 = c ^ "-" ^ (name_of_color color) ^ "-" in
    let s2 = c ^ (string_of_cards cards) in
    let s3 = c ^ (string_of_cost inventory) in
    let s4 = c ^ (string_of_trophies trophies) in
      (s1, s2, s3, s4)
  in
  let lines = List.rev (List.map strings_of_players players) in
  let (s1, s2, s3, s4) = 
    List.fold_left (fun (aa,bb,cc,dd) (a,b,c,d) -> (a::aa,b::bb,c::cc,d::dd)) ([],[],[],[]) lines in
  let p s n = List.nth s n in
  let p1 = p s1 in
  let p2 = p s2 in
  let p3 = p s3 in
  let p4 = p s4 in
  let sp = "     " in
  let format = format_of_string "%-52s %-52s %-52s %-52s \n" in
    (sprintf format (sp ^ (p1 0)) (sp ^ (p1 1)) (sp ^ (p1 2)) (sp ^ (p1 3))) ^
    (sprintf format (p2 0) (p2 1) (p2 2) (p2 3)) ^
    (sprintf format (p3 0) (p3 1) (p3 2) (p3 3)) ^
    (sprintf format (p4 0) (p4 1) (p4 2) (p4 3))


(* Assumes move is valid, prints whatever the move says *)
let print_move (s : state) (c : color) (m : move) : string =
  let code = code_of_color c in
  let player = string_of_color c in
  let string_of_robbermove ((_,p,_,_) : state) ((piece, stealfrom) : robbermove) : string = 
    "the robber to hex " ^ (soi piece) 
      ^ (match stealfrom with 
          | Some(from) -> 
            let player_from = List.find (fun (c,_,_) -> c = from) p in
            let (_,(inv,_),_) = player_from in
              if (sum_cost inv) > 0 then 
                " and stealing from " ^ (code_of_color from) ^ (string_of_color from)
              else ""
          | None -> "")
  in
  let msg = (
    match m with
      | InitialMove(p1, p2) -> 
        "initialized town at " ^ (soi p1) ^ " with road to " ^ (soi p2)
      | RobberMove(robbermove) -> "moved " ^ (string_of_robbermove s robbermove)
      | DiscardMove(cost) -> "discarded " ^ (string_of_cost cost)
      | TradeResponse(resp) -> (if resp then "accepted " else "rejected ") ^ "the trade"
      | Action(RollDice) -> let (_, _, turn, _) = s in
        "rolled " ^ (soi (match turn.dicerolled with Some(r) -> r | None -> failwith "tried to print dice roll of None."))
      | Action(MaritimeTrade(sell, buy)) -> "bartered " ^ (string_of_resource sell) ^ " for " ^ (string_of_resource buy)
      | Action(DomesticTrade(color2, cost1, cost2)) ->
        let player2 = string_of_color color2 in
          "requested trade with " ^ player2 ^ ": " ^ (string_of_cost cost1) ^ " for " ^ (string_of_cost cost2)
      | Action(BuyBuild(build)) -> "built a " ^
        (match build with
          | BuildRoad((c, (p1, p2))) -> "road from " ^ (soi p1) ^ " to " ^ (soi p2)
          | BuildTown(point) -> "town at point " ^ (soi point)
          | BuildCity(point) -> "city at point " ^ (soi point)
          | BuildCard -> "development card")
      | Action(PlayCard(playcard)) -> "played " ^
        (match playcard with
          | PlayKnight(robbermove) -> "a Knight, moving " ^ (string_of_robbermove s robbermove)
          | PlayRoadBuilding((_, (p1, p2)), road2opt) -> "Road Building " ^
            (match road2opt with
              | None -> "to build a road from " ^ (soi p1) ^ " to " ^ (soi p2)
              | Some(_, (p3, p4)) ->
                "to build roads from " ^ (soi p1) ^ " to " ^ (soi p2) ^ " and from " ^ (soi p3) ^ " to " ^ (soi p4))
          | PlayYearOfPlenty(res1, res2opt) -> "Year of Plenty for " ^
            (match res2opt with 
              | None -> (string_of_resource res1) 
              | Some(res2) -> (string_of_resource res1) ^ " and " ^ (string_of_resource res2))
          | PlayMonopoly(monopoly) -> "a Monopoly on " ^ (string_of_resource monopoly))
      | Action(EndTurn) -> let c = next_turn c in 
        "ended the turn. " ^ (code_of_color c) ^ (string_of_color c) ^ "'s turn begins")
  in
    (code ^ player ^ " " ^ msg ^ ".")


(* move transcript adequately describes the turn state *)
let print_state (s : state) : string =
  let (board, players, _, _) = s in
    (print_board board) ^ (print_players players) ^ (print_test s)


(* prints copypaste-able state *)
let print_code_of_state s =
  let (((hexes, ports), (intersections, roads), deck, discard, robber), players, turn, next) = s in
  let foldin f l =
    if List.length l = 0 then "[]"
    else "[" ^ (List.fold_left f "" l) ^ "\b\b  \b\b]"
  in
  let fold_hexes a (t, i) = a ^ (sprintf "(%s, %d); " (string_of_terrain t) i) in
  let fold_ports a ((p1,p2), i, pr) =
    a ^ sprintf "((%d, %d), %d, %s); " p1 p2 i 
    (match pr with
      | Any -> "Any"
      | PortResource r -> sprintf "PortResource %s" (string_of_resource r))
  in
  let fold_intersections a = function
    | None -> a ^ "None; "
    | Some (c, t) -> a ^ (sprintf "Some(%s, %s); " (string_of_color c) (string_of_settlement t))
  in
  let fold_roads a (c, (p1, p2)) = a ^ (sprintf "(%s, (%d, %d)); " (string_of_color c) p1 p2) in
  let fold_cardlist a c = a ^ (string_of_card_full c) ^ "; " in
  let code_cardlist l = (foldin fold_cardlist l) in
  let code_cost (i1,i2,i3,i4,i5) = sprintf "(%d, %d, %d, %d, %d)" i1 i2 i3 i4 i5 in
  let code_players ps = 
    (* To prevent last \n *)
    let rec rec_players a l =
      match l with 
        | [] -> ""
        | (c, (inv, cards), (t1,t2,t3))::t ->
        let a = a ^ (sprintf "(%s, (%s, Reveal %s), (%d, %b, %b));"
          (string_of_color c) (code_cost inv) (code_cardlist (reveal cards)) t1 t2 t3)
        in
          (if t = [] then a else (rec_players (a ^ "\n") t))
    in
      "[" ^ (rec_players "" ps) ^ "]"
  in
  let code_robber = soi in
  let code_turn t = 
    sprintf "{active = %s; dicerolled = %s; cardplayed = %b; cardsbought = Reveal %s; tradesmade = %d; pendingtrade = %s}"
      (string_of_color t.active)
      (match t.dicerolled with None -> "None" | Some i -> "Some " ^ (soi i))
      t.cardplayed
      (code_cardlist (reveal t.cardsbought))
      t.tradesmade
      (match t.pendingtrade with 
        | None -> "None"
        | Some (c, c1, c2) -> sprintf "Some (%s, %s, %s)" (string_of_color c) (code_cost c1) (code_cost c2))
  in
  let code_next (c, r) = sprintf "(%s, %s)" (string_of_color c) (string_of_request r) in
  let str = "\n\n"
    ^ "(((" ^ (foldin fold_hexes hexes) ^ ",\n"
    ^ (foldin fold_ports ports) ^ "),\n"
    ^ "(" ^ (foldin fold_intersections intersections) ^ ",\n"
    ^ (foldin fold_roads roads) ^ "),\n"
    ^ "Reveal " ^ (code_cardlist (reveal deck)) ^ ",\n"
    ^ (code_cardlist discard) ^ ",\n"
    ^ (code_robber robber) ^ "),\n"
    ^ (code_players players) ^ ",\n"
    ^ (code_turn turn) ^ ",\n"
    ^ (code_next next) ^ ")"
  in
    print str; ignore (read_line())


let printer (s : string) : unit = print (clear ^ s); flush_all()



(*------------------------------ Play Print ----------------------------------*)

(* Deliver results in play *)
let print_winner (c : color) : unit = print (">> Winner:\t" ^ (string_of_color c))
let print_draw () = print ">> Draw"

let print_hbar () =
  print "-----------------------------------------------------------"

let print_count (s : string) (n : int) : unit = print_endline ("\t>>  " ^ s ^ ":\t" ^ (soi n) ^ "  <<")





(******************************************************************************)
(** {2 Interface}                                                             *)
(******************************************************************************)

(**
 * This is the only function you need to call in game!
 * Call with updated s after m has been made by c
 *)
let print_update (c : color) (m : move) (s : state) : unit =
  if not (to_print()) then () else
    printer ((print_move s c m) ^ (print_state s))



