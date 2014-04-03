open Async.Std
  
open Reader
  
open Pipe
  
open Protocol
  
module Make (Job : MapReduce.Job) =
  struct
    (* Pipe for processing requests *)
    let (reqreader, reqwriter) = Pipe.create ()
      
    (* Pipe for processing responses *)
    let (resreader, reswriter) = Pipe.create ()
      
    (* Listens to controller and enques request onto request pipe if found *)
    let queue_message r =
      (WorkerRequest.receive r) >>=
        (function
         | `Eof -> return ()
         | `Ok message -> write reqwriter message)
      
    let run r w = return (don't_wait_for (queue_message r))
      
  end
  
(* see .mli *)
let init port =
  (Tcp.Server.create ~on_handler_error: `Raise (Tcp.on_port port)
     (fun _ r w ->
        (read_line r) >>=
          (function
           | `Eof -> return ()
           | `Ok job ->
               (match MapReduce.get_job job with
                | None -> return ()
                | Some j -> let module Job = (val j)
                    in let module Mapper = Make(Job) in Mapper.run r w))))
    >>=
    (fun _ ->
       (print_endline "server started";
        print_endline "worker started.";
        print_endline "registered jobs:";
        List.iter print_endline (MapReduce.list_jobs ());
        never ()))
  
