open Async.Std

let hosts_ports : (string * int) list ref = ref []

type worker = Reader.t * Writer.t

let init addrs = hosts_ports := addrs

module Make (Job : MapReduce.Job) = struct

  exception InfrastructureFailure
  exception MapFailed of string
  exception ReduceFailed of string

  module C = Combiner.Make(Job)
  module W = Worker.Make(Job)
  module WRequest = Protocol.WorkerRequest(Job)
  module WResponse = Protocol.WorkerResponse(Job)
  
  (***************************************************************************)
  (* keep track of current avaliable workers.                                *)
  (***************************************************************************)
  let workers = Hashtbl.create 10 (* binds a unique int to each worker *)
  let rev_workers = Hashtbl.create 10 (* holds the reverse binings of hastable workers*)
  let current = ref 0  (* current avaliable worker *)
  let size = ref 0 (* total number of workers that were ever added. Remain unchanged when remove_worker is called *)
  
  let add_worker (r, w) = 
    let id = !size in
    size := !size + 1;
    Hashtbl.add workers id (r, w);
    Hashtbl.add rev_workers (r, w) id 
  
  let remove_worker (r, w) = 
    Hashtbl.remove workers (Hashtbl.find rev_workers (r, w))
  
  let rec get_next_worker () = 
    if Hashtbl.length workers = 0 then raise InfrastructureFailure
    else begin 
      let x = !current in
      current := (!current + 1) mod !size;
      try Hashtbl.find workers x with Not_found -> get_next_worker ()
    end
  (***************************************************************************)

  (* connect to all workers with given hosts and ports in lst, send them 
   * the job name, and add them to the hashtable workers *)
  let connect_to_all (lst : (string * int) list) = 
    let name = Job.name in
    let connect host port = 
      Tcp.connect (Tcp.to_host_and_port host port) >>= fun (_, r, w) ->
        Writer.write_line w name;
        return (add_worker (r, w))
    in 
      Deferred.List.map lst (fun (host, port) -> connect host port)


  (* sends a map request to worker (r, w) with input i. return the map result *)
  let rec map_job i (r, w) = 
    try
      WRequest.send w (WRequest.MapRequest i);
      WResponse.receive r >>= function 
        | `Ok (WResponse.JobFailed msg) -> raise (MapFailed msg)
        | `Ok (WResponse.MapResult results) -> return results    
        | _ -> remove_worker (r, w); map_job i (get_next_worker ()) 
    with 
    | MapFailed msg -> raise (MapFailed msg)
    | _ -> map_job i (get_next_worker ())

  (* sends a reduce request to worker (r, w) with input (k, interLst). 
   * return the reduce result *)
  let rec reduce_job (k, interLst) (r, w) = 
    try
      WRequest.send w (WRequest.ReduceRequest (k, interLst));
      WResponse.receive r >>= function 
        | `Ok (WResponse.JobFailed msg) -> raise (ReduceFailed msg)
        | `Ok (WResponse.ReduceResult results) -> return (k, results)    
        | _ -> remove_worker (r, w); reduce_job (k, interLst) (get_next_worker ())
    with
    | ReduceFailed msg -> raise (ReduceFailed msg)
    | _ -> reduce_job (k, interLst) (get_next_worker ())
    
  (* get the map results of inputs from workers *)
  let map inputs = 
    Deferred.List.map inputs (fun x -> map_job x (get_next_worker()))

  (* get the reduce results of inters from workers *)
  let reduce inters = 
    Deferred.List.map inters (fun x -> reduce_job x (get_next_worker()))

  (* see mli *)
  let map_reduce inputs =
    connect_to_all !hosts_ports >>= fun _ ->
      map inputs 
      >>| List.flatten
      >>| C.combine
      >>= fun inters -> reduce inters

end


