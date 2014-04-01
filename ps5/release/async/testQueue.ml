#require "async";;
#use "aQueue.ml";;
open Async.Std;;

let test () = 
let q = create () in 
ignore (pop q >>= fun v -> print_endline (string_of_int v); return ());
ignore (pop q >>= fun v -> print_endline (string_of_int v); return ());
ignore (pop q >>= fun v -> print_endline (string_of_int v); return ());
ignore (pop q >>= fun v -> print_endline (string_of_int v); return ());
ignore (pop q >>= fun v -> print_endline (string_of_int v); return ());
ignore (pop q >>= fun v -> print_endline (string_of_int v); return ());
ignore (pop q >>= fun v -> print_endline (string_of_int v); return ());
ignore (pop q >>= fun v -> print_endline (string_of_int v); return ());
ignore (pop q >>= fun v -> print_endline (string_of_int v); return ());
push q 1;
push q 2;
push q 3;
push q 4;
push q 5;
push q 6;
push q 7;
push q 8;
push q 9;;

(** Run the examples *)
let _ = test () 

(** Start the async scheduler *)
let _ =
  Scheduler.go ()