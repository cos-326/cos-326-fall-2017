type base = A | T | C | G
type dna = base list

let eq (b1:base) (b2 : base) : bool = (b1 = b2)

exception NotBase
let to_char b = 
  match b with A -> 'A' | T -> 'T' | C -> 'C' | G -> 'G'

let from_char b = 
  match b with 'A' -> A | 'T' -> T | 'C' -> C | 'G' -> G | _ -> raise NotBase


let compare (a:base) (b:base) : int =
  match (a,b) with
      (A, A) | (T, T) | (C, C) | (G, G) -> 0
    | (A, _) -> -1 | (_, A) -> 1
    | (T, _) -> -1 | (_, T) -> 1
    | (C, _) -> -1 | (_, C) -> 1


let dna_from_string (s : string) : dna =
  let rec aux s n length =
    if n < length then
      from_char (s.[n]) :: aux s (n+1) length
    else
      []
  in
  aux s 0 (String.length s)


let iterator f xs =
  let rec aux f n xs =
    match xs with
	[] -> ()
      | hd::tail -> f n hd; aux f (n+1) tail
  in
  aux f 0 xs


let dna_to_string (bs : dna) : string =
  let result = Bytes.create (List.length bs) in
  iterator (fun i b -> result.[i] <- to_char b) bs;
  result


let longer_dna_of (a:dna) (b:dna) : dna =
  if List.length a <= List.length b then b else a

