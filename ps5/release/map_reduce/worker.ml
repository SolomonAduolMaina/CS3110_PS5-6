open Async.Std
  
module Make (Job : MapReduce.Job) =
  struct
    module ReqChannel = Protocol.WorkerRequest(Job)
      
    module ResChannel = Protocol.WorkerResponse(Job)
      
    (* Process map request *)
    let process_map input =
      try_with (fun () -> 
        (Job.map input) >>= (fun list -> return (ResChannel.MapResult list))
      ) >>= 
        function
        | Core.Std.Ok x -> return x
        | Core.Std.Error e -> return (ResChannel.JobFailed (Printexc.get_backtrace ()))
      
    (* Process reduce request *)
    let process_reduce (key, list) =
      try_with (fun () -> 
        (Job.reduce (key, list)) >>=
          (fun output -> return (ResChannel.ReduceResult output))
      ) >>= 
        function
        | Core.Std.Ok x -> return x
        | Core.Std.Error e -> return (ResChannel.JobFailed (Printexc.get_backtrace ()))
      
    (* Process a request sent from the controller *)
    let process_request request =
      match request with
      | ReqChannel.MapRequest input -> process_map input
      | ReqChannel.ReduceRequest (k, l) -> process_reduce (k, l)
      
    (* Handle the requests for a single connection *)
    let rec run reader writer =
      Printexc.record_backtrace true;
      try_with (fun () -> 
        ReqChannel.receive reader >>=
          function
           | `Eof -> return ()
           | `Ok request ->
               (process_request request >>= fun result ->
                  return (ResChannel.send writer result))
      ) >>= 
        function
        | Core.Std.Ok _ -> run reader writer
        | Core.Std.Error _ -> 
            (Deferred.both (Writer.close writer) (Reader.close reader) >>= 
              fun _ -> return () )
      
  end
  
(* see .mli *)
let init port =
  (Tcp.Server.create ~on_handler_error: `Raise (Tcp.on_port port)
     (fun _ r w ->
        (Reader.read_line r) >>=
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
  
