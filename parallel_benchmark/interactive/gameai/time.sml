structure GTime :> GTIME =
struct

type gtime = real

val start = ref NONE
val speed = ref 1.0

fun init () =
    start := SOME (Time.now ())

fun setspeed (s: real) =
    speed := s

fun now () =
    case !start of
        NONE => 0.0
      | SOME t =>
        ((Real.fromLargeInt (Time.toSeconds (Time.- (Time.now (), t)))) * (!speed))
        / 60.0

fun toMinutes (t: gtime) : real =
    t

fun fromMinutes (t: real) : gtime =
    t

fun fromSeconds (t: real) : gtime =
    t / 60.0

fun within (intv: gtime) (t1, t2) =
    Real.abs (t1 - t2) < intv

val cw = cwithin
fun cwithin (intv: gtime) (t1, t2) =
    cw intv (t1, t2)

fun toTime (t: gtime) : Time.time =
    let val msecs = IntInf.fromInt (Real.round (t * 60.0 * 1000.0 / (!speed)))
    in
        Time.fromMilliseconds msecs
    end

fun fromTime (t: Time.time) : gtime =
    (Real.fromLargeInt (Time.toMilliseconds t)) * (!speed) / 60.0 / 1000.0

val plus = Real.+
val minus = Real.-
val gt = Real.>
val lt = Real.<

fun toString t =
    let val {whole = mins, frac = mfrac} = Real.split t
        val secs = mfrac * 60.0
        val mins = Real.round mins
        val secs = Real.round secs
        val (mins, secs) = if secs >= 60 then (mins + 1, 0) else (mins, secs)
    in
        (Int.toString mins) ^ (if secs < 10 then ":0" else ":") ^
        (Int.toString secs)
    end

fun sleep (t: gtime) : unit =
    let val rem = fromTime (Posix.Process.sleep (toTime t))
    in
        if rem < 0.0001 then
            ()
        else
            sleep rem
    end

end
