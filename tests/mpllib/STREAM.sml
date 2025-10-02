signature STREAM =
sig
  type 'a t
  type 'a stream = 'a t

  val nth: 'a stream -> int -> 'a

  val tabulate: (int -> 'a) -> 'a stream
  val map: ('a -> 'b) -> 'a stream -> 'b stream
  val mapIdx: (int * 'a -> 'b) -> 'a stream -> 'b stream
  val zipWith: ('a * 'b -> 'c) -> 'a stream * 'b stream -> 'c stream
  val iteratePrefixes: ('b * 'a -> 'b) -> 'b -> 'a stream -> 'b stream
  val iteratePrefixesIncl: ('b * 'a -> 'b) -> 'b -> 'a stream -> 'b stream

  val applyIdx: int * 'a stream -> (int * 'a -> unit) -> unit
  val iterate: ('b * 'a -> 'b) -> 'b -> int * 'a stream -> 'b

  val pack: ('a -> 'b option) -> (int * 'a stream) -> 'b ArraySlice.slice

  val makeBlockStreams:
    { blockSize: int
    , numChildren: int
    , offset: int -> int
    , getElem: int -> int -> 'a
    }
    -> (int -> 'a stream)
end
