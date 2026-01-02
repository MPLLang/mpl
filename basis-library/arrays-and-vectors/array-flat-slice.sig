signature ARRAY_FLAT_SLICE =
sig
  type 'a slice

  val all: ('a -> bool) -> 'a slice -> bool
  val app : ('a -> unit) -> 'a slice -> unit
  val appi: (int * 'a -> unit) -> 'a slice -> unit
  val base: 'a slice -> 'a ArrayFlat.t * int * int
  val collate: ('a * 'a -> order) -> 'a slice * 'a slice -> order
  val copy: {dst: 'a ArrayFlat.t, di: int, src: 'a slice} -> unit
  val copyVec: {dst: 'a ArrayFlat.t, di: int, src: 'a VectorFlatSlice.slice} -> unit
  val exists: ('a -> bool) -> 'a slice -> bool
  val find: ('a -> bool) -> 'a slice -> 'a option
  val findi: (int * 'a -> bool) -> 'a slice -> (int * 'a) option
  val foldl: ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val foldli: (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val foldr: ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val foldri: (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
  val full: 'a ArrayFlat.t -> 'a slice
  val getItem: 'a slice -> ('a * 'a slice) option
  val isEmpty: 'a slice -> bool
  val length: 'a slice -> int
  val modify : ('a -> 'a) -> 'a slice -> unit
  val modifyi: (int * 'a -> 'a) -> 'a slice -> unit
  val slice: 'a ArrayFlat.t * int * int option -> 'a slice
  val sub: 'a slice * int -> 'a
  val subslice: 'a slice * int * int option -> 'a slice
  val update: 'a slice * int * 'a -> unit
  val vector: 'a slice -> 'a VectorFlat.vector
end

signature ARRAY_FLAT_SLICE_EXTRA =
sig
  include ARRAY_FLAT_SLICE

  val uninitIsNop: 'a slice -> bool
  val uninit: 'a slice * int -> unit
  val unsafeSub: 'a slice * int -> 'a
  val unsafeCopy: {dst: 'a ArrayFlat.t, di: int, src: 'a slice} -> unit
  val unsafeCopyVec: {dst: 'a ArrayFlat.t, di: int, src: 'a VectorFlatSlice.slice} -> unit
  val unsafeSlice: 'a ArrayFlat.t * int * int option -> 'a slice
  val unsafeSubslice: 'a slice * int * int option -> 'a slice
  val unsafeUninit: 'a slice * int -> unit
  val unsafeUpdate: 'a slice * int * 'a -> unit

  val concat: 'a slice list -> 'a ArrayFlat.t
  val toList: 'a slice -> 'a list
end
