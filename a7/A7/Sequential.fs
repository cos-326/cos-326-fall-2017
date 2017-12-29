  (*******************************************************)
  (* Sequential Sequences Based on a List Representation *)
  (*******************************************************)
module Sequential.S 
  type 'a t = Rep of 'a list

  let (!) = fun (Rep l) -> l

  let from_array (a:'a array) = Rep (Array.toList a)

  let to_array (Rep l) = List.toArray l

  let length (Rep l) = List.length l

  let tabulate f n =
    let rec helper acc x =
      if x = n then List.rev acc
      else helper ((f x)::acc) (x+1) in
    Rep (helper [] 0)

  let nth s i = List.item i !s

  let scan f b (Rep l) =
      match List.scan f b l with
         | hd::tl -> Rep tl    // discard the first element of List.scan to get our scan
         | _ -> failwith "impossible scan"

  let filter f (Rep l) = Rep (List.filter f l)

  let map f (Rep l) = Rep (List.map f l)

  let iter f (Rep l) = List.iter f l

  let reduce (f : 'a -> 'a -> 'a) acc (Rep l) = List.fold f acc l 


