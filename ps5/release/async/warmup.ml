open Async.Std

let fork d f1 f2 =
	ignore (d >>= fun v -> Deferred.both (f1 v) (f2 v))

let deferred_map l f =
  failwith "XQA"


