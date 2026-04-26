signature ARRAY_AOS_SLICE =
sig
  type 'a slice

  val all: ('a -> bool) -> 'a slice -> bool
  val app : ('a -> unit) -> 'a slice -> unit
  val appi: (int * 'a -> unit) -> 'a slice -> unit
  val base: 'a slice -> 'a ArrayAos.t * int * int
  val collate: ('a * 'a -> order) -> 'a slice * 'a slice -> order
  val copy: {dst: 'a ArrayAos.t, di: int, src: 'a slice} -> unit
  val copyVec: {dst: 'a ArrayAos.t, di: int, src: 'a VectorAosSlice.slice} -> unit
  val exists: ('a -> bool) -> 'a slice -> bool
  val find: ('a -> bool) -> 'a slice -> 'a option
  val findi: (int * 'a -> bool) -> 'a slice -> (int * 'a) option
  val foldl: ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val foldr: ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val foldri: (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val full: 'a ArrayAos.t -> 'a slice
  val getItem: 'a slice -> ('a * 'a slice) option
  val isEmpty: 'a slice -> bool
  val length: 'a slice -> int
  val modify : ('a -> 'a) -> 'a slice -> unit
  val modifyi: (int * 'a -> 'a) -> 'a slice -> unit
  val slice: 'a ArrayAos.t * int * int option -> 'a slice
  val sub: 'a slice * int -> 'a
  val subslice: 'a slice * int * int option -> 'a slice
  val update: 'a slice * int * 'a -> unit
  val vector: 'a slice -> 'a VectorAos.vector
end

signature ARRAY_AOS_SLICE_EXTRA =
sig
  include ARRAY_AOS_SLICE

  val uninitIsNop: 'a slice -> bool
  val uninit: 'a slice * int -> unit
  val unsafeSub: 'a slice * int -> 'a
  val unsafeCopy: {dst: 'a ArrayAos.t, di: int, src: 'a slice} -> unit
  val unsafeCopyVec: {dst: 'a ArrayAos.t, di: int, src: 'a VectorAosSlice.slice} -> unit
  val unsafeSlice: 'a ArrayAos.t * int * int option -> 'a slice
  val unsafeSubslice: 'a slice * int * int option -> 'a slice
  val unsafeUninit: 'a slice * int -> unit
  val unsafeUpdate: 'a slice * int * 'a -> unit

  val concat: 'a slice list -> 'a ArrayAos.t
  val toList: 'a slice -> 'a list
end
