#require "async";;
#use "aQueue.ml";;
open Async.Std;;

let test () = 
let q = create () in 
push q 1;
push q 2;
push q 3;
push q 4;
push q 5;
push q 6;
push q 7;
push q 8;
ignore (pop q >>= fun v -> print_endline (string_of_int v); return ());
ignore (pop q >>= fun v -> print_endline (string_of_int v); return ());
ignore (pop q >>= fun v -> print_endline (string_of_int v); return ());
ignore (pop q >>= fun v -> print_endline (string_of_int v); return ());
ignore (pop q >>= fun v -> print_endline (string_of_int v); return ());
ignore (pop q >>= fun v -> print_endline (string_of_int v); return ());
ignore (pop q >>= fun v -> print_endline (string_of_int v); return ());
ignore (pop q >>= fun v -> print_endline (string_of_int v); return ());

pop q >>= fun v -> print_endline (string_of_int v); return ();;

(** Run the examples *)
let _ = test () >>> fun () -> shutdown 0

(** Start the async scheduler *)
let _ =
  Scheduler.go ()