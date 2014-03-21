(* A mutex to fix access the non-threadsafe Re_str module *)

module D = Debug.Make(struct let name = "re_fix" end)
open D

let re_mutex = Mutex.create ()
let exec why f x = 
  debug "REFIX getting lock (%s)" why;
  Mutex.lock re_mutex;
  debug "REFIX got lock (%s)" why;
  try 
    let result = f x in
    debug "REFIX releasing lock (%s)" why;
    Mutex.unlock re_mutex;
    debug "REFIX released lock (%s)" why;
    result
  with e ->
    debug "REFIX caught exception: releasing lock (%s)" why;
    Mutex.unlock re_mutex;
    debug "REFIX caught exception: released lock (%s)" why;
    raise e
