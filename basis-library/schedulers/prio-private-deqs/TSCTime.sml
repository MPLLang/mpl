structure TSCTime :>
sig
  type t

  val toTime : t -> Time.time
  val fromTime : Time.time -> t

  val zeroTime : t
  val fromMicroseconds : LargeInt.int -> t
  val toMicroseconds : t -> LargeInt.int
  val now : unit -> t

  val + : t * t -> t
  val - : t * t -> t
  val > : t * t -> bool
end =
struct

  type time = Word64.word
  type t = time

  val now = _import "Time_rdtsc" private : unit -> Word64.word;

  val zeroTime = Word64.fromInt 0

  (* experimentally determined for PBBS. Seems to be the same for Aware. *)
  val cyclesPerMicrosecond = 0w2400

  fun fromMicroseconds us =
    Word64.* (Word64.fromLargeInt us, cyclesPerMicrosecond)

  fun toMicroseconds us =
    Word64.toLargeInt (Word64.div (us, cyclesPerMicrosecond))

  fun toTime us =
    Time.fromMicroseconds (toMicroseconds us)

  fun fromTime t =
    fromMicroseconds (Time.toMicroseconds t)

  val op+ = Word64.+
  val op- = Word64.-
  val op> = Word64.>
end
