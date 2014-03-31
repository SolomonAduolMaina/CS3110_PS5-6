open Async.Std
open Pipe

(* a pipe with a size *)
type 'a t = 'a Reader.t * 'a Writer.t * int ref

let create () = let (r,w) = create () in (r, w, ref 0)    

let push ((r, w, s): 'a t) x = 
  s := !s + 1;
  don't_wait_for (write w x)

let pop ((r, w, s): 'a t) = 
  if !s = 0 then failwith "Empty Queue"
  else begin
    s := !s - 1;
    read r >>= fun result -> match result with 
      | `Eof -> failwith "<pop> EOF"
      | `Ok x -> return x
  end

