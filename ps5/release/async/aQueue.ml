open Async.Std
open Pipe

(* a pipe with a size *)
type 'a t = 'a Reader.t * 'a Writer.t

let create () : 'a t = Pipe.create ()   

let push ((r, w): 'a t) x = don't_wait_for (write w x)

let pop ((r, w): 'a t) = 
  read r >>= fun result -> 
  match result with 
    | `Eof -> failwith "<pop> EOF"
    | `Ok x -> return x