signature SEQUENCE_G =
sig
  type 'a t
  type 'a seq = 'a t
  type 'a ord = 'a * 'a -> order
  datatype 'a listview = NIL | CONS of 'a * 'a seq
  datatype 'a treeview = EMPTY | ONE of 'a | PAIR of 'a seq * 'a seq

  exception Range
  exception Size

  val nth : 'a seq -> int -> 'a
  val length : 'a seq -> int
  val toList : int -> 'a seq -> 'a list
  val toString : ('a -> string) -> 'a seq -> string
  val equal : int -> ('a * 'a -> bool) -> 'a seq * 'a seq -> bool

  val empty : unit -> 'a seq
  val singleton : 'a -> 'a seq
  val tabulate : int -> (int -> 'a) -> int -> 'a seq (**)
  val fromList : 'a list -> 'a seq

  val rev : int -> 'a seq -> 'a seq
  val append : int -> 'a seq * 'a seq -> 'a seq (**)
  val flatten : int -> 'a seq seq -> 'a seq

  val filter : int -> ('a -> bool) -> 'a seq -> 'a seq
  val map : int -> ('a -> 'b) -> 'a seq -> 'b seq (**)
  val zip : int -> 'a seq * 'b seq -> ('a * 'b) seq
  val zipWith : int -> ('a * 'b -> 'c) -> 'a seq * 'b seq -> 'c seq

  val enum : int -> 'a seq -> (int * 'a) seq
  val filterIdx : int -> (int * 'a -> bool) -> 'a seq -> 'a seq
  val mapIdx : int -> (int * 'a -> 'b) -> 'a seq -> 'b seq
  val update : int -> 'a seq * (int * 'a) -> 'a seq
  val inject : int -> 'a seq * (int * 'a) seq -> 'a seq

  val subseq : int -> 'a seq -> int * int -> 'a seq
  val take : int -> 'a seq -> int -> 'a seq
  val drop : int -> 'a seq -> int -> 'a seq
  val splitHead : int -> 'a seq -> 'a listview
  val splitMid : int -> 'a seq -> 'a treeview

  val iterate : int -> ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b
  val iteratePrefixes : int -> ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b seq * 'b
  val iteratePrefixesIncl : int -> ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b seq
  val reduce : int -> ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a (**)
  val scan : int -> ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a seq * 'a
  val scanIncl : int -> ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a seq

  val sort : int -> 'a ord -> 'a seq -> 'a seq (**)
  val merge : int -> 'a ord -> 'a seq * 'a seq -> 'a seq (**)
  val collect : int -> 'a ord -> ('a * 'b) seq -> ('a * 'b seq) seq
  val collate : int -> 'a ord -> 'a seq ord
  val argmax : int -> 'a ord -> 'a seq -> int

  val $ : 'a -> 'a seq
  val % : 'a list -> 'a seq
end
