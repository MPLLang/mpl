(* Faster time implementation; especially better*)
structure FTime :>
sig
  type t

  val now : unit -> t
  val zeroTime : t
  val fromNanoseconds : LargeInt.int -> t
  val fromMilliseconds : LargeInt.int -> t
  val fromMicroseconds : LargeInt.int -> t
  val fromSeconds : LargeInt.int -> t

  val + : t * t -> t
  val - : t * t -> t

  val < : t * t -> bool
  val > : t * t -> bool
  val = : t * t -> bool
  val <= : t * t -> bool
  val >= : t * t -> bool
end =
struct
  open Word64

  type t = word

  val now = _import "Parallel_get_nanoseconds" runtime private: unit -> t;

  val zeroTime = 0w0 : t

  fun fromNanoseconds x = Word64.fromLargeInt x
  fun fromMicroseconds x = 0w1000 * fromNanoseconds x
  fun fromMilliseconds x = 0w1000000 * fromNanoseconds x
  fun fromSeconds x = 0w1000000000 * fromNanoseconds x

  fun op= (t1, t2) = (t1 = t2)
end
