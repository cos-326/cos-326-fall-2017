  (*******************************************************)
  (* Parallel Sequences Based on an Array Representation *)
  (*******************************************************)
module Parallel.S 
  type 'a t = Rep of 'a array   

  let fail() = failwith "unimplemented"

  let from_array (a:'a array) : 'a t = fail()
  
  let to_array (Rep a) = Array.copy a

  let length (Rep a) = a.Length

  let tabulate (f:int -> 'a) (n:int) : 'a t = fail()

  let nth (Rep a) i = a.[i]

  let map (f:'a -> 'b) (s:'a t) : 'b t = fail()

  let iter f (Rep a) = Array.iter f a

  let reduce (f:'a -> 'a -> 'a) (b:'a) (s:'a t) : 'a = fail ()
 
