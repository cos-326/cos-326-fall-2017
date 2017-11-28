module type MEMOIZER =
sig
  (* the type of the memoized function's argument *)
  type key

  (* memo f returns a memoized version of f.
   * assumes f r x makes recursive calls by calling r not f *)
  val memo : ((key -> 'a) -> (key -> 'a)) -> (key -> 'a)
end

module Memoizer : functor (D : Map.S) -> MEMOIZER with type key = D.key
