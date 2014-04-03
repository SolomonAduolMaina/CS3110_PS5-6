open Async.Std
  
open Reader
  
module Make (Job : MapReduce.Job) =
  struct
    open Pipe
      
    module ReqChannel = Protocol.WorkerRequest(Job)
      
    module ResChannel = Protocol.WorkerResponse(Job)
      
    (* Pipe for processing requests *)
    let (reqreader, reqwriter) = Pipe.create ()
      
    (* Pipe for processing responses *)
    let (resreader, reswriter) = Pipe.create ()
      
    (* Listen to controller and enque request onto request pipe if found *)
    let receive_message r : unit Deferred.t =
      (ReqChannel.receive r) >>=
        (function
         | `Eof -> return ()
         | `Ok message -> write reqwriter message)
      
    (* Process map request and enqueue output to response pipe *)
    let process_map input : unit Deferred.t =
      (Job.map input) >>=
        (fun list -> Pipe.write reswriter (ResChannel.MapResult list))
      
    (* Process reduce request and enqueue output to response pipe *)
    let process_reduce (key, list) : unit Deferred.t =
      (Job.reduce (key, list)) >>=
        (fun output -> Pipe.write reswriter (ResChannel.ReduceResult output))
      
    (* Pass message to apporopriate processing channel *)
    let process_message () : unit Deferred.t =
      (Pipe.read reqreader) >>=
        (function
         | `Eof -> return ()
         | `Ok request ->
             (match request with
              | ReqChannel.MapRequest input -> process_map input
              | ReqChannel.ReduceRequest (k, l) -> process_reduce (k, l)))
      
    (* Send response back to controller *)
    let send_response w : unit Deferred.t =
      (Pipe.read resreader) >>=
        (function
         | `Eof -> return ()
         | `Ok result -> return (ResChannel.send w result))
      
    (** Handle the requests for a single connection.  The Reader and Writer should
      be used to send and receive messages of type WorkerResponse(Job) and
      WorkerRequest(Job). *)
    let rec run r w =
      (don't_wait_for (receive_message r);
       don't_wait_for (process_message ());
       don't_wait_for (send_response w);
       run r w)
      
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
  
