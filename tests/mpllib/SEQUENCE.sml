signature SEQUENCE =
sig
  type 'a t
  type 'a seq = 'a t
  (* type 'a ord = 'a * 'a -> order
  datatype 'a listview = NIL | CONS of 'a * 'a seq
  datatype 'a treeview = EMPTY | ONE of 'a | PAIR of 'a seq * 'a seq *)

  (* exception Range
  exception Size *)

  val nth: 'a seq -> int -> 'a
  val length: 'a seq -> int
  val toList: 'a seq -> 'a list
  val toString: ('a -> string) -> 'a seq -> string
  val equal: ('a * 'a -> bool) -> 'a seq * 'a seq -> bool

  val empty: unit -> 'a seq
  val singleton: 'a -> 'a seq
  val tabulate: (int -> 'a) -> int -> 'a seq
  val fromList: 'a list -> 'a seq

  val rev: 'a seq -> 'a seq
  val append: 'a seq * 'a seq -> 'a seq
  val flatten: 'a seq seq -> 'a seq

  val map: ('a -> 'b) -> 'a seq -> 'b seq
  val mapOption: ('a -> 'b option) -> 'a seq -> 'b seq
  val zip: 'a seq * 'b seq -> ('a * 'b) seq
  val zipWith: ('a * 'b -> 'c) -> 'a seq * 'b seq -> 'c seq
  val zipWith3: ('a * 'b * 'c -> 'd) -> 'a seq * 'b seq * 'c seq -> 'd seq

  val filter: ('a -> bool) -> 'a seq -> 'a seq
  (* val filterSome: 'a option seq -> 'a seq *)
  val filterIdx: (int * 'a -> bool) -> 'a seq -> 'a seq

  val enum: 'a seq -> (int * 'a) seq
  val mapIdx: (int * 'a -> 'b) -> 'a seq -> 'b seq
  (* val update: 'a seq * (int * 'a) -> 'a seq *)
  val inject: 'a seq * (int * 'a) seq -> 'a seq

  val subseq: 'a seq -> int * int -> 'a seq
  val take: 'a seq -> int -> 'a seq
  val drop: 'a seq -> int -> 'a seq
  (* val splitHead: 'a seq -> 'a listview *)
  (* val splitMid: 'a seq -> 'a treeview *)

  val iterate: ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b
  (* val iteratePrefixes: ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b seq * 'b *)
  (* val iteratePrefixesIncl: ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b seq *)
  val reduce: ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a
  val scan: ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a seq * 'a
  val scanIncl: ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a seq

  (* val sort: 'a ord -> 'a seq -> 'a seq
  val merge: 'a ord -> 'a seq * 'a seq -> 'a seq
  val collect: 'a ord -> ('a * 'b) seq -> ('a * 'b seq) seq
  val collate: 'a ord -> 'a seq ord
  val argmax: 'a ord -> 'a seq -> int *)

  val $ : 'a -> 'a seq
  val % : 'a list -> 'a seq

  val fromArraySeq: 'a ArraySlice.slice -> 'a seq
  val toArraySeq: 'a seq -> 'a ArraySlice.slice

  val force: 'a seq -> 'a seq

  val applyIdx: 'a seq -> (int * 'a -> unit) -> unit

  (* val foreach: 'a seq -> (int * 'a -> unit) -> unit
  val foreachG: int -> 'a seq -> (int * 'a -> unit) -> unit *)
end
