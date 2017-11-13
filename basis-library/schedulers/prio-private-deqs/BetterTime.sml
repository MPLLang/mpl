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

  type time = SysWord.word * SysWord.word
  type t = time

  val getTimeOfDay = _import "Time_getTimeOfDay" private : (C_Time.t) ref * (C_SUSeconds.t) ref -> C_Int.t;

  fun now () =
    let
      val secr = ref (C_Time.castFromFixedInt 0)
      val usecr = ref (C_SUSeconds.castFromFixedInt 0)
      val _ = getTimeOfDay (secr, usecr)
    in
      ( C_Time.castToSysWord (!secr) 
      , C_SUSeconds.castToSysWord (!usecr)
      )
    end

  val zeroTime = (SysWord.fromInt 0, SysWord.fromInt 0)

  fun fromMicroseconds us =
    ( SysWord.fromLargeInt (LargeInt.quot (us, 1000000))
    , SysWord.fromLargeInt (LargeInt.rem (us, 1000000))
    )

  local val tli = SysWord.toLargeInt in
  fun toMicroseconds (s, us) =
    LargeInt.+ (LargeInt.* (1000000, tli s), tli us)
  end

  fun toTime t = Time.fromMicroseconds (toMicroseconds t)

  fun fromTime t =
    fromMicroseconds (Time.toMicroseconds t)

  fun add ((s1, us1), (s2, us2)) =
    (SysWord.+ (s1, s2), SysWord.+ (us1, us2))

  fun sub ((s1, us1), (s2, us2)) =
    (SysWord.- (s1, s2), SysWord.- (us1, us2))

  fun gt ((s1, us1), (s2, us2)) =
    case SysWord.compare (s1, s2) of
      LESS => false
    | GREATER => true
    | EQUAL => SysWord.> (us1, us2)

  val op+ = add
  val op- = sub
  val op> = gt
end
