module Sequential.S
    type 'a t
    val from_array : 'a array -> 'a t 
    val to_array : 'a t -> 'a array
    val length : 'a t -> int
    val tabulate : (int -> 'a) -> int -> 'a t
    val nth : 'a t -> int -> 'a
    val map : ('a -> 'b) -> 'a t -> 'b t
    val iter : ('a -> unit) -> 'a t -> unit
    val reduce : ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a
