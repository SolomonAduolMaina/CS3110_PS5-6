open Async.Std
open Async_unix

(******************************************************************************)
(** input and output types                                                    *)
(******************************************************************************)

type id = int
type dna_type = Read | Ref

type sequence = {
  id   : id;
  kind : dna_type;
  data : string;
}

(** Indicates a matching subsequence of the given read and reference *)
type result = {
  length   : int;

  read     : id;
  read_off : int;

  ref      : id;
  ref_off  : int;
}

(******************************************************************************)
(** file reading and writing                                                  *)
(******************************************************************************)

(** Convert a line into a sequence *)
let read_sequence line = match Str.split (Str.regexp "@") line with
  | [id; "READ"; seq] -> {id=int_of_string id; kind=Read; data=seq}
  | [id; "REF";  seq] -> {id=int_of_string id; kind=Ref;  data=seq}
  | _ -> failwith "malformed input"

(** Read in the input data *)
let read_files filenames : sequence list Deferred.t =
  if filenames = [] then failwith "No files supplied"
  else
    Deferred.List.map filenames Reader.file_lines
      >>| List.flatten
      >>| List.map read_sequence


(** Print out a single match *)
let print_result result =
  printf "read %i [%i-%i] matches reference %i [%i-%i]\n"
         result.read result.read_off (result.read_off + result.length -1)
         result.ref  result.ref_off  (result.ref_off  + result.length -1)

(** Write out the output data *)
let print_results results : unit =
  List.iter print_result results

(******************************************************************************)
(** Dna sequencing jobs                                                       *)
(******************************************************************************)

module S = String

(* identifying information for a 10-mer*)
type identify_info = {
  source : sequence;
  offset : int;
}

(* a match between a 10-mer read and a 10-mer ref *)
type mers_match = {
  read     : id;
  read_off : int;

  ref      : id;
  ref_off  : int;
}

module Job1 = struct
  type input = sequence
  type key = string
  type inter = identify_info
  type output = mers_match list

  let name = "dna.job1"

  let map input : (key * inter) list Deferred.t =
    let str = input.data in
    let rec helper start x out = 
      if x < 10 then out
      else helper (start+1) (x-1) ((S.sub str start 10, {source=input; offset=start}) :: out)
    in
      return (helper 0 (S.length str) [])

  (* split a list of inters into two lists of reads sources and refs sources *)
  let split_inters inters = 
    let f (reads, refs) elem =
      match elem.source.kind with 
      | Read -> ((elem::reads), refs)
      | Ref  -> (reads, (elem::refs))
    in
      List.fold_left f ([],[]) inters

  let reduce (key, inters) : output Deferred.t = 
    let (reads, refs) = split_inters inters in 
    let create_match read ref = 
      {read=read.source.id; read_off=read.offset;
       ref=ref.source.id; ref_off=ref.offset;}
    in
    let f reads acc ref = 
      List.fold_left (fun acc read -> (create_match read ref) :: acc) acc reads
    in
      return (List.fold_left (f reads) [] refs)
end

let () = MapReduce.register_job (module Job1)


module Job2 = struct
  type input  = mers_match
  (* read id, ref id *)
  type key    = id * id
  (* read offset, ref offset *)
  type inter  = int * int
  type output = result list

  let name = "dna.job2"

  let map input : (key * inter) list Deferred.t =
    return [((input.read, input.ref), (input.read_off, input.ref_off))]


  let reduce ((read_id, ref_id), inters) : output Deferred.t =
    let sorted_inters = List.sort (fun (x, _) (y, _) -> compare y x) inters in
    let create_result (read_off, ref_off) : result = 
      {length=10; read=read_id; read_off=read_off; ref=ref_id; ref_off=ref_off;}
    in
    let results = List.fold_left (fun acc elem -> create_result elem :: acc) [] sorted_inters
    in
    let merge (r1: result) (r2 : result) : result = 
      {length=r2.length + (r2.read_off-r1.read_off); 
      read=r1.read; read_off=r1.read_off; ref=r1.ref; ref_off=r1.ref_off;}
    in 
    let are_mergable (r1: result) (r2 : result) : bool = 
      (r2.read_off - r1.read_off <= r1.length) &&
      (r2.read_off - r1.read_off = r2.ref_off - r1.ref_off)
    in
    let rec helper working out = 
      match working, out with
      | [], _ -> out
      | h::t, [] -> helper t [h]
      | h2::t2, h1::t1 -> begin
        if are_mergable h1 h2 then helper t2 (merge h1 h2 :: t1)
        else helper t2 (h2::out) 
      end
    in
      return (helper results [])

end

let () = MapReduce.register_job (module Job2)



module App  = struct

  let name = "dna"

  module Make (Controller : MapReduce.Controller) = struct
    module MR1 = Controller(Job1)
    module MR2 = Controller(Job2)

    let run (input : sequence list) : result list Deferred.t =
      MR1.map_reduce input 
      >>| List.fold_left (fun acc elem -> snd elem :: acc) [] 
      >>| List.flatten
      >>= MR2.map_reduce 
      >>| List.fold_left (fun acc elem -> snd elem :: acc) [] 
      >>| List.flatten

    let main args =
      read_files args
        >>= run
        >>| print_results
  end
end

let () = MapReduce.register_app (module App)

