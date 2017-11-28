module type QUEUE = sig
  type q

  val emp : q
  val ins : int * q -> q
  val rem : q -> (int * q) option 
end

module M1 = struct
  type q = int list * int list

  let emp = ([], [])

  let ins (i,q) =
    let (front, back) = q in
    (front, i::back)

  let rem q =
    match q with
      ([],[]) -> None
    | (hd::front, back) -> Some (hd, (front,back))
    | ([], back) -> 
      (match List.rev back with
         hd::tail -> Some (hd, (tail, []))
       | _ -> failwith "impossible")       
end

module M2 = struct
  type q = int list

  let emp = []

  let ins (i,q) = q @ [i]

  let rem q =
    match q with
      [] -> None
    | hd::tail -> Some (hd, tail)
end

(***************************************)
(* Fill this in, then copy to queue.txt*)
(***************************************)
let abstract (q : int list * int list) : int list =
  failwith "unimplemented"

let client1 emp ins rem =
  emp

let client2 emp ins rem =
  ins (5, ins (3, emp)) 

let client3 emp ins rem =
  let q1 = ins (5, ins (3, emp)) in
  let q2 = 
    match rem q1 with 
      Some (hd,q2) -> q2
    |_ -> failwith "impossible"
  in
  ins (7, q2)

(* Tests that client code preserves the relation between M1 and M2 *)
let _ =
  assert(abstract (client1 M1.emp M1.ins M1.rem) = client1 M2.emp M2.ins M2.rem);
  assert(abstract (client2 M1.emp M1.ins M1.rem) = client2 M2.emp M2.ins M2.rem);
  assert(abstract (client3 M1.emp M1.ins M1.rem) = client3 M2.emp M2.ins M2.rem)
