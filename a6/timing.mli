(* return the runtime, in s, of arg1 called on arg2 *)
val time_fun : ('a -> 'b) -> 'a -> float

(* return the average runtime, in s, over arg3 trials of arg1 called on arg2 *)
val iter_time_fun : ('a -> 'b) -> 'a -> int -> float
