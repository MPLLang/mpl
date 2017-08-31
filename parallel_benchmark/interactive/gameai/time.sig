signature GTIME =
sig

    type gtime

    val init: unit -> unit
    val setspeed : real -> unit

    val now: unit -> gtime

    val plus : gtime * gtime -> gtime
    val minus : gtime * gtime -> gtime
    val gt : gtime * gtime -> bool
    val lt : gtime * gtime -> bool

    val within : gtime -> gtime * gtime -> bool
    val cwithin : gtime -> gtime * gtime -> order

    val toMinutes : gtime -> real
    val fromMinutes : real -> gtime
    val fromSeconds : real -> gtime

    val toString : gtime -> string

    val sleep : gtime -> unit

end
