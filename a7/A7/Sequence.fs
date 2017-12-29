  (***********************************)
  (* Sequences to be used by clients *)
  (***********************************)
module Sequence.S

open Sequential
//open Parallel

  type 'a t = R of 'a S.t

  let from_array s = R (S.from_array s)

  let to_array (R s) = S.to_array s

  let length (R s) = S.length s

  let tabulate f n = R (S.tabulate f n)

  let nth (R s) i = S.nth s i

  let map f (R s) = R (S.map f s)

  let iter f (R s) = S.iter f s

  let reduce (f : 'a -> 'a -> 'a) acc (R s) = S.reduce f acc s 

  
