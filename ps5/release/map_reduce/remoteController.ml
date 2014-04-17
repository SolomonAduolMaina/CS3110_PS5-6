open Async.Std
  
let hosts_ports : ((string * int) list) ref = ref []
  
type worker = (Reader.t * Writer.t)

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
  (* binds a unique int to each worker *)
  let workers = Hashtbl.create 10
  (* holds the reverse binings of hastable workers *)    
  let rev_workers = Hashtbl.create 10
  (* current avaliable worker *)
  let current = ref 0
  (** total number of workers that were ever added. Remain unchanged when 
    * remove_worker is called *)
  let size = ref 0
    
  let add_worker (r, w) =
    let id = !size
    in
      (size := !size + 1;
       Hashtbl.add workers id (r, w);
       Hashtbl.add rev_workers (r, w) id)
    
  let remove_worker (r, w) =
    don't_wait_for (Reader.close r); don't_wait_for (Writer.close w);
    Hashtbl.remove workers (Hashtbl.find rev_workers (r, w))
    
  let rec get_next_worker () =
    if (Hashtbl.length workers) = 0
    then raise InfrastructureFailure
    else
      (let x = !current
       in
         (current := (!current + 1) mod !size;
          try Hashtbl.find workers x with | Not_found -> get_next_worker ()))

  (***************************************************************************)
  
  (**  connect to all workers with given hosts and ports in lst, send them 
    * the job name, and add them to the hashtable workers *)
  let connect_to_all (lst : (string * int) list) =
    let name = Job.name in
    let connect host port =
      try_with (fun () ->
        (Tcp.connect (Tcp.to_host_and_port host port)) >>=
        (fun (_, r, w) ->
           (Writer.write_line w name; return (add_worker (r, w))))
      ) >>= fun _ -> return ()
    in 
      Deferred.List.map lst (fun (host, port) -> connect host port)

  (* close all open connections with workers *)
  let close_all_connections () =
    let close x (r, w) = 
      don't_wait_for (Reader.close r); don't_wait_for (Writer.close w)
    in 
      Hashtbl.iter close workers 
    
  let rec send_map (r, w) input table =
    let f () =
      let () = WRequest.send w (WRequest.MapRequest input) in return (r, w)
    in
      (try_with f) >>=
        (function
         | Core.Std.Ok (r, w) ->
             (match Hashtbl.mem table (r, w) with
              | true ->
                  let old = Hashtbl.find table (r, w)
                  in return (old := input :: !old)
              | false -> return (Hashtbl.add table (r, w) (ref [ input ])))
         | Core.Std.Error exn ->
             let () = remove_worker (r, w)
             in send_map (get_next_worker ()) input table)
    
  let rec get_map inters deg ((r, w), jobs) =
    match jobs with
    | [] -> return ((r, w), (inters, deg))
    | input :: xs ->
        let f () =
          (WResponse.receive r) >>=
            (function
             | `Ok (WResponse.JobFailed msg) -> raise (MapFailed msg)
             | `Ok (WResponse.MapResult r) -> return ((r :: inters), deg)
             | funny_message -> return (inters, (input :: deg)))
        in
          (try_with ~extract_exn:true f) >>=
            (function
             | Core.Std.Ok (ins, degs) -> get_map ins degs ((r, w), xs)
             | Core.Std.Error exn ->
                 (match exn with
                  | MapFailed msg -> raise (MapFailed msg)
                  | noread -> get_map inters (input :: deg)) ((r, w), xs))
    
  let rec map inputs =
    let table = Hashtbl.create 10 in
    let ping input = send_map (get_next_worker ()) input table in
    let sent_all = Deferred.List.iter ~how: `Sequential inputs ~f: ping
    in
      sent_all >>=
        (fun () ->
           let f (r, w) jobs all = ((r, w), (List.rev !jobs)) :: all in
           let l = Hashtbl.fold f table [] in
           let l' = Deferred.List.map ~how: `Parallel l ~f: (get_map [] [])
           in l' >>= union_map)

  and union_map results =
    let f (gss, dss) ((r, w), (gs, ds)) =
      match ds with
      | [] -> return ((gs :: gss), dss)
      | _ ->
          let () = remove_worker (r, w)
          in return ((gs :: gss), (ds :: dss))
    in
      (Deferred.List.fold results ([], []) f) >>=
        (fun (inters, degs) ->
           let gs = List.flatten inters in
           let ds = List.flatten degs
           in
             match ds with
             | [] -> return gs
             | _ -> (map ds) >>= (fun l -> return (List.rev_append gs l)))
    
  let rec send_reduce (r, w) (k, l) table =
    let f () =
      let () = WRequest.send w (WRequest.ReduceRequest (k, l))
      in return (r, w)
    in
      (try_with f) >>=
        (function
         | Core.Std.Ok (r, w) ->
             (match Hashtbl.mem table (r, w) with
              | true ->
                  let old = Hashtbl.find table (r, w)
                  in return (old := (k, l) :: !old)
              | false -> return (Hashtbl.add table (r, w) (ref [ (k, l) ])))
         | Core.Std.Error exn ->
             let () = remove_worker (r, w)
             in send_reduce (get_next_worker ()) (k, l) table)
    
  let rec get_reduce gs ds ((r, w), jobs) =
    match jobs with
    | [] -> return ((r, w), (gs, ds))
    | (k, l) :: xs ->
        let f () =
          (WResponse.receive r) >>=
            (function
             | `Ok (WResponse.JobFailed msg) -> raise (MapFailed msg)
             | `Ok (WResponse.ReduceResult o) ->
                 return (((k, o) :: gs), ds)
             | funny_message -> return (gs, ((k, l) :: ds)))
        in
          (try_with ~extract_exn:true f) >>=
            (function
             | Core.Std.Ok (os, degs) -> get_reduce os degs ((r, w), xs)
             | Core.Std.Error exn ->
                 (match exn with
                  | MapFailed msg -> raise (MapFailed msg)
                  | noread -> get_reduce gs ((k, l) :: ds)) ((r, w), xs))
    
  let rec reduce inters =
    let table = Hashtbl.create 10 in
    let ping (k, l) = send_reduce (get_next_worker ()) (k, l) table in
    let sent_all = Deferred.List.iter ~how: `Sequential inters ~f: ping
    in
      sent_all >>=
        (fun () ->
           let f (r, w) jobs all = ((r, w), (List.rev !jobs)) :: all in
           let l = Hashtbl.fold f table [] in
           let f = get_reduce [] [] in
           let l' = Deferred.List.map ~how: `Parallel l ~f: f
           in l' >>= union_reduce)

  and union_reduce results =
    let f (outputss, faileds) ((r, w), (outputs, failed)) =
      match failed with
      | [] -> return ((outputs :: outputss), faileds)
      | _ ->
          let () = remove_worker (r, w)
          in return ((outputs :: outputss), (failed :: faileds))
    in
      (Deferred.List.fold results ([], []) f) >>=
        (fun (outputss, faileds) ->
           let os = List.flatten outputss in
           let fs = List.flatten faileds
           in
             match fs with
             | [] -> return os
             | _ ->
                 (reduce fs) >>= (fun l -> return (List.rev_append os l)))
    
  let map_reduce inputs =
    connect_to_all !hosts_ports 
    >>= fun _ -> map inputs
    >>| List.flatten 
    >>| C.combine 
    >>= reduce
    >>= fun results -> close_all_connections (); return results
    
end
  
