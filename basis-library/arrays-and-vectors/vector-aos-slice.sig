signature VECTOR_AOS_SLICE =
sig
  type 'a slice

  val length: 'a slice -> int
  val sub: 'a slice * int -> 'a
  val full: 'a VectorAos.t -> 'a slice
  val slice: 'a VectorAos.t * int * int option -> 'a slice
  val subslice: 'a slice * int * int option -> 'a slice
  val base: 'a slice -> 'a VectorAos.t * int * int
  val vector: 'a slice -> 'a VectorAos.t
  val concat: 'a slice list -> 'a VectorAos.t
  val isEmpty: 'a slice -> bool
  val getItem: 'a slice -> ('a * 'a slice) option
  val appi: (int * 'a -> unit) -> 'a slice -> unit
  val app: ('a -> unit) -> 'a slice -> unit
  val mapi: (int * 'a -> 'b) -> 'a slice -> 'b VectorAos.t
  val map: ('a -> 'b) -> 'a slice -> 'b VectorAos.t
  val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val foldl: ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val foldri: (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val foldr: ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val findi: (int * 'a -> bool) -> 'a slice -> (int * 'a) option
  val find: ('a -> bool) -> 'a slice -> 'a option
  val exists: ('a -> bool) -> 'a slice -> bool
  val all: ('a -> bool) -> 'a slice -> bool
  val collate: ('a * 'a -> order) -> 'a slice * 'a slice -> order
end

signature VECTOR_AOS_SLICE_EXTRA =
sig
  include VECTOR_AOS_SLICE

  val copy: {dst: 'a ArrayAos.t, di: int, src: 'a slice} -> unit

  val unsafeSub: 'a slice * int -> 'a
  val unsafeCopy: {dst: 'a ArrayAos.t, di: int, src: 'a slice} -> unit
  val unsafeSlice: 'a VectorAos.t * int * int option -> 'a slice
  val unsafeSubslice: 'a slice * int * int option -> 'a slice

  (* Used to implement Substring/String functions *)
  val concatWith: 'a VectorAos.t -> 'a slice list -> 'a VectorAos.t
  val triml: int -> 'a slice -> 'a slice
  val trimr: int -> 'a slice -> 'a slice
  val isPrefix: ('a * 'a -> bool) -> 'a VectorAos.t -> 'a slice -> bool
  val isSubvector: ('a * 'a -> bool) -> 'a VectorAos.t -> 'a slice -> bool
  val isSuffix: ('a * 'a -> bool) -> 'a VectorAos.t -> 'a slice -> bool
  val splitl: ('a -> bool) -> 'a slice -> 'a slice * 'a slice
  val splitr: ('a -> bool) -> 'a slice -> 'a slice * 'a slice
  val splitAt: 'a slice * int -> 'a slice * 'a slice
  val dropl: ('a -> bool) -> 'a slice -> 'a slice
  val dropr: ('a -> bool) -> 'a slice -> 'a slice
  val takel: ('a -> bool) -> 'a slice -> 'a slice
  val taker: ('a -> bool) -> 'a slice -> 'a slice
  val position: ('a * 'a -> bool)
                -> 'a VectorAos.t
                -> 'a slice
                -> 'a slice * 'a slice
  val span: ''a slice * ''a slice -> ''a slice
  val translate: ('a -> 'b VectorAos.t) -> 'a slice -> 'b VectorAos.t
  val tokens: ('a -> bool) -> 'a slice -> 'a slice list
  val fields: ('a -> bool) -> 'a slice -> 'a slice list

  val toList: 'a slice -> 'a list
end
