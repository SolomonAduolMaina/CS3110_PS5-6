open Async.Std
  
open Reader
  
module Make (Job : MapReduce.Job) =
  struct
    module ReqChannel = Protocol.WorkerRequest(Job)
      
    module ResChannel = Protocol.WorkerResponse(Job)
      
    (* Receive request from controller *)
    let rec receive_request reader =
      (ReqChannel.receive reader) >>=
        (function
         | `Eof -> receive_request reader
         | `Ok request -> return request)
      
    (* Process map request *)
    let process_map input =
      (Job.map input) >>= (fun list -> return (ResChannel.MapResult list))
      
    (* Process reduce request *)
    let process_reduce (key, list) =
      (Job.reduce (key, list)) >>=
        (fun output -> return (ResChannel.ReduceResult output))
      
    (* Process a request send from the server *)
    let process_request request =
      match request with
      | ReqChannel.MapRequest input -> process_map input
      | ReqChannel.ReduceRequest (k, l) -> process_reduce (k, l)
      
    (* Send response back to controller *)
    let send_result writer result =
      (ResChannel.send writer result; return ())
      
    (* Handle the requests for a single connection *)
    let rec run reader writer =
      (receive_request reader) >>=
        (fun request ->
           (process_request request) >>=
             (fun result ->
                (send_result writer result) >>= (fun () -> run reader writer)))
      
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
  
