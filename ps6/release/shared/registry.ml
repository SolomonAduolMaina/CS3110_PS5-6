(** Register bots. You don't need to worry about this. *)

open Definition

module type Soul = sig end

module New = struct end

(* Create a BOT by giving your bot functor a New : Soul *)
module type BOT = sig
  val initialize : unit -> unit
  val handle_request : state -> move
end


(* List of registered bots *)
let registry : (string * (module BOT)) list ref = ref []


(* register four instances of the same bot *)
let register_bot name b1 b2 b3 b4 : unit =
  let r = !registry in
    if List.exists (fun (n, _) -> n = name) r then () 
    else let b = [name, b1; name, b2; name, b3; name, b4] in registry := (b@r)


(* mem-remove a bot from the registry by name *)
let bot_of_name (name : string) : (module BOT) =
  let rec find l =
    match l with 
      | [] -> failwith ("Unregistered bot: " ^ name)
      | (n, b)::t -> if n = name then b, t
        else let (b', t) = find t in (b', ((n, b)::t))
  in
    let b, r = find (!registry) in
      registry := r; b
