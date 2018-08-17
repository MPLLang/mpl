structure BetterTime :>
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

  type time = SysWord.word
  type t = time

  val getTimeOfDay = _import "Time_getTimeOfDay" private : (C_Time.t) ref * (C_SUSeconds.t) ref -> C_Int.t;

  fun now () =
    let
      val secr = ref (C_Time.castFromFixedInt 0)
      val usecr = ref (C_SUSeconds.castFromFixedInt 0)
      val _ = getTimeOfDay (secr, usecr)
    in
      SysWord.+
        ( SysWord.* (0w1000000, C_Time.castToSysWord (!secr))
        , C_SUSeconds.castToSysWord (!usecr)
        )
    end

  val zeroTime = SysWord.fromInt 0

  fun fromMicroseconds us =
    SysWord.fromLargeInt us

  fun toMicroseconds us =
    SysWord.toLargeInt us

  fun toTime us =
    Time.fromMicroseconds (toMicroseconds us)

  fun fromTime t =
    fromMicroseconds (Time.toMicroseconds t)

  val op+ = SysWord.+
  val op- = SysWord.-
  val op> = SysWord.>
end
