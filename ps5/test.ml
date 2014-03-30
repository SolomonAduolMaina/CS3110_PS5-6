let prog () = 
  let f i = 
    printf "Value %d\n" i;
    return () in 
  Deferred.both (Deferred.List.iter [1;2;3;4;5] f) (Deferred.List.iter [1;2;3;4;5] f)
