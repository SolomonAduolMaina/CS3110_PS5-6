open Async.Std

let hosts_ports : (string * int) list ref = ref []

type worker = Reader.t * Writer.t

let init addrs = hosts_ports := addrs

module Make (Job : MapReduce.Job) = struct

  module C = Combiner.Make(Job)

  let workers = Hashtbl.create 10
  let current = ref 0 
  let add_worker (r, w) = Hashtbl.add workers (Hashtbl.length workers) (r, w)
  let get_next_worker () =
    let x = !current in 
      current := (!current + 1) mod (Hashtbl.length workers);
      Hashtbl.find workers x

  (* connect to the worker with the given host and port, and adds it to workers. *)
  let connect (host : string) (port : int) : unit Deferred.t = 
  	Tcp.connect (Tcp.to_host_and_port host port) >>= fun (_, r, w) -> return (add_worker (r, w))
  
  (* connect to all the workers with the given hosts and ports in lst, and adds them to workers *)
  let rec connect_to_all (lst : (string * int) list) : unit Deferred.t = 
    match lst with
    | [] -> return ()
    | (host, port)::t -> connect host port >>= fun () -> connect_to_all t

  (* sends a map request to worker (r, w) with input i. return the map result *)
  let map_job i (r, w) = failwith "<map_job> NOT IMPLEMENTED "

  (* sends a reduce request to worker (r, w) with input i. return the reduce result *)
  let reduce_job i (r, w) = failwith "<reduce_job> NOT IMPLEMENTED "

  (* get the map results of inputs from workers *)
  let map inputs = 
    let rec helper inputs results = 
      match inputs with 
      | [] -> return results
      | h::t -> helper t ((map_job h (get_next_worker ())) :: results)
    in
      helper inputs []

  (* get the reduce results of inters from workers *)
  let reduce inters = 
    let rec helper inters results = 
      match inters with 
      | [] -> return results
      | h::t -> helper t ((reduce_job h (get_next_worker ())) :: results)
    in
      helper inters []

  (* see mli *)
  let map_reduce inputs =
    connect_to_all !hosts_ports >>= fun () ->
      map inputs 
      >>| List.flatten
      >>| C.combine
      >>= fun l -> reduce l

end

