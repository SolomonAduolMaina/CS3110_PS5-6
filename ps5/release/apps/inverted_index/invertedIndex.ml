open Async.Std
  
open Async_unix
  
type filename = string

(******************************************************************************)
(** {2 The Inverted Index Job}                                                *)
(******************************************************************************)
module Job =
  struct
    type file_name = string
    
    type contents = string
    
    type word = string
    
    type input = (file_name * contents)
    
    type key = word
    
    type inter = file_name
    
    type output = file_name list
    
    let name = "index.job"
      
    (* For each word in file filename, map that word to the filename *)
    let map (filename, contents) : ((key * inter) list) Deferred.t =
      let words = AppUtils.split_words contents in
      let f word_list word = (word, filename) :: word_list
      in return (List.fold_left f [] words)
      
    (* Remove duplicates from the list of filenames in which word key occurs *)
    let reduce (key, inters) : output Deferred.t =
      let module M = Map.Make(String)
      in
        let f table inter =
          if M.mem inter table then table else M.add inter None table in
        let g inter_list (inter, opt) = inter :: inter_list in
        let pairs = M.bindings (List.fold_left f M.empty inters)
        in return (List.fold_left g [] pairs)
      
  end
  
(* register the job *)
let () = MapReduce.register_job (module Job)
  
(******************************************************************************)
(** {2 The Inverted Index App}                                                *)
(******************************************************************************)
module App =
  struct
    let name = "index"
      
    (** Print out all of the documents associated with each word *)
    let output results =
      let print (word, documents) =
        (print_endline (word ^ ":");
         List.iter (fun doc -> print_endline ("    " ^ doc)) documents) in
      let sorted = List.sort compare results in List.iter print sorted
      
    (** for each line f in the master list, output a pair containing the filename
      f and the contents of the file named by f.  *)
    let read master_file : ((filename * string) list) Deferred.t =
      (Reader.file_lines master_file) >>=
        (fun filenames ->
           Deferred.List.map filenames
             (fun filename ->
                (Reader.file_contents filename) >>=
                  (fun contents -> return (filename, contents))))
      
    module Make (Controller : MapReduce.Controller) =
      struct
        module MR = Controller(Job)
          
        (** The input should be a single file name.  The named file should contain
        a list of files to index. *)
        let main args =
          match args with
          | [] -> failwith "No file provided."
          | [ x ] -> ((read (List.hd args)) >>= MR.map_reduce) >>| output
          | _ -> failwith "Too many files provided"
          
      end
      
  end
  
(* register the App *)
let () = MapReduce.register_app (module App)
  
