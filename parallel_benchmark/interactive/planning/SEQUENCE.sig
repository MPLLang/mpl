signature SEQUENCE =
sig
  type 'a seq
  type 'a ord = 'a * 'a -> order
  datatype 'a listview = NIL | CONS of 'a * 'a seq
  datatype 'a treeview = EMPTY | ONE of 'a | PAIR of 'a seq * 'a seq

  exception Range
  exception Size

  val nth : 'a seq -> int -> 'a
  val length : 'a seq -> int
  val toList : 'a seq -> 'a list
  val toString : ('a -> string) -> 'a seq -> string
  val equal : ('a * 'a -> bool) -> 'a seq * 'a seq -> bool

  val empty : unit -> 'a seq
  val singleton : 'a -> 'a seq
  val tabulate : (int -> 'a) -> int -> 'a seq
  val fromList : 'a list -> 'a seq

  val rev : 'a seq -> 'a seq
  val append : 'a seq * 'a seq -> 'a seq
  val flatten : 'a seq seq -> 'a seq

  val filter : ('a -> bool) -> 'a seq -> 'a seq
  val map : ('a -> 'b) -> 'a seq -> 'b seq
(*  val map2 : ('a * 'b -> 'c) -> 'a seq -> 'b seq -> 'c seq*)
  val zip : 'a seq * 'b seq -> ('a * 'b) seq

  val enum : 'a seq -> (int * 'a) seq
  val filterIdx : ((int * 'a) -> bool) -> 'a seq -> 'a seq
  val mapIdx : ((int * 'a) -> 'b) -> 'a seq -> 'b seq
  val inject : (int * 'a) seq -> 'a seq -> 'a seq

  val subseq : 'a seq -> int * int -> 'a seq
  val take : 'a seq -> int -> 'a seq
  val drop : 'a seq -> int -> 'a seq
(*  val showl : 'a seq -> 'a listview*)
(*  val showt : 'a seq -> 'a treeview*)

(*  val iter : ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b*)
(*  val iterh : ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b seq * 'b*)
  val reduce : ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a
  val scan : ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a seq * 'a
(*  val scani : ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a seq*)

  val sort : 'a ord -> 'a seq -> 'a seq
  val merge : 'a ord -> 'a seq * 'a seq -> 'a seq
  val collect : 'a ord -> ('a * 'b) seq -> ('a * 'b seq) seq
  val collate : 'a ord -> 'a seq ord
  val argmax : 'a ord -> 'a seq -> int

  val % : 'a list -> 'a seq
end
