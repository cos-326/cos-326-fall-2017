type base = A | T | C | G
type dna = base list

val eq : base -> base -> bool

exception NotBase

val to_char : base -> char

(* raise NotBase when char not one of A T C G *)
val from_char : char -> base   

(* an arbitrary total order over bases *)
(* 0 is equal; -1 is less than; 1 is greater than *)
val compare : base -> base -> int

(* converts a string to a list of bases *)
(* raise NotBase when string contains a char that is not one of A T C G *)
val dna_from_string : string -> dna  

(* converts a list of bases to a string *)
val dna_to_string : dna -> string

(* Given DNA strands a and b, returns the longer one. *)
val longer_dna_of : dna -> dna -> dna
