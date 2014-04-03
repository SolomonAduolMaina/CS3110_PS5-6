open Async.Std
  
open Reader
  
module Make (Job : MapReduce.Job) =
  struct
    open Pipe
      
    module ReqChan = Protocol.WorkerRequest(Job)
      
    module ResChan = Protocol.WorkerResponse(Job)
      
    (* Pipe for processing requests *)
    let (reqreader, reqwriter) = Pipe.create ()
      
    (* Pipe for processing responses *)
    let (resreader, reswriter) = Pipe.create ()
      
    (* Listen to controller and enque request onto request pipe if found *)
    let queue_message r : unit Deferred.t =
      (ReqChan.receive r) >>=
        (function
         | `Eof -> return ()
         | `Ok message -> write reqwriter message)
      
    (* Process map request and enqueue output to response pipe *)
    let process_map input = return ()
      
    (* Process reduce request and enqueue output to response pipe *)
    let process_reduce (key, list) = return ()
      
    (* Pass message to apporopriate processing ... process? *)
    let process_message () : unit Deferred.t =
      (Pipe.read reqreader) >>=
        (fun request ->
           match request with
           | `Eof -> return ()
           | `Ok req ->
               (match req with
                | ReqChan.MapRequest input -> process_map input
                | ReqChan.ReduceRequest (k, l) -> process_reduce (k, l)))
      
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
  
