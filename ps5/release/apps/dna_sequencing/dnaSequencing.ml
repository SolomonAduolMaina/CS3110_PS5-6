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
  let f r1 r2 =
    match r1.read - r2.read, r1.read_off - r2.read_off, 
      r1.ref - r2.ref, r1.ref_off - r2.ref_off with 
    | 0, 0, 0, x 
    | 0, 0, x, _
    | 0, x, _, _
    | x, _, _, _ -> x
  in
  let sorted = List.sort f results in
  List.iter print_result sorted

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
    let create_result (read_off, ref_off) : result = 
      {length=10; read=read_id; read_off=read_off; ref=ref_id; ref_off=ref_off;}
    in

    let results = List.fold_left (fun acc elem -> create_result elem :: acc) [] inters
    in

    let rec merge_all h lst = 
      let merge (x1: result) (x2 : result) : result = 
        let (r1, r2) = 
          if x2.read_off - x1.read_off >= 0 then (x1, x2) 
          else (x2, x1)
        in 
          {length= (max (r1.length + r1.read_off) (r2.length + r2.read_off)) - r1.read_off; 
          read=r1.read; read_off=r1.read_off; ref=r1.ref; ref_off=r1.ref_off;}
      in 
        match lst with 
        | [] -> h
        | x::t -> merge_all (merge h x) t
    in 

    let are_mergeable (x1: result) (x2 : result) : bool =
      let (r1, r2) = 
        if x2.read_off - x1.read_off >= 0 then (x1, x2) 
        else (x2, x1)
      in 
        (r2.read_off - r1.read_off <= r1.length) &&
        (r2.read_off - r1.read_off = r2.ref_off - r1.ref_off)
    in

    let check_and_merge (h : 'a) (lst : 'a list) : 'a * 'a list * bool = 
      let (mergable, not_mergeable) = List.partition (are_mergeable h) lst 
      in 
        match mergable with 
        | [] -> (h, lst, false)
        | _ -> (merge_all h mergable, not_mergeable, true)
    in

    let rec helper working out  = 
      match working with
      | [] -> out
      | h::t -> begin
        match check_and_merge h t with 
        | x, lst, false -> helper lst (x::out)
        | x, lst, true -> helper (x::lst) out
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

