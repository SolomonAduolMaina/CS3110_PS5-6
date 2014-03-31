open Async.Std

let fork d f1 f2 =
  ignore (d >>= fun v -> Deferred.both (f1 v) (f2 v))

let deferred_map (l : 'a list) ( f : 'a -> 'b Deferred.t) =
  List.fold_left 
  (fun acc ele -> ele >>= fun v -> acc >>= fun v' -> return (v::v'))
  (return []) (List.fold_left (fun acc ele -> (f ele)::acc) [] l)
