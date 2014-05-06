open Definition
open Constant

(* return a higher int value if the probability of rolling this roll is higher *)
let get_roll_probablity = function
	| 6 | 8  -> 5
	| 5 | 9  -> 4 
	| 4 | 10 -> 3
	| 3 | 11 -> 2
	| 2 | 12 -> 1
	| _      -> 0

(* = 0 if two rolls have equal probability. 
   > 0 if the roll1 has more probability.
   < 0 if roll2 has more probability *)
let compare_rolls roll1 roll2 =
  get_roll_probablity roll1 - get_roll_probablity roll2