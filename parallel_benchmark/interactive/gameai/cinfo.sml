structure CInfo =
struct

type cinfo =
     { patience: real,
       confusion: real,
       boldness: int,
       expertise: int,
       friendliness: int }

fun within dist (r1, r2) =
    Real.abs (r1 - r2) < dist

fun eq dist ((c1, c2): cinfo * cinfo) =
    (within dist (#patience c1, #patience c2)) andalso
    (within dist (#confusion c1, #confusion c2)) andalso
    (#boldness c1 = #boldness c2) andalso
    (#expertise c1 = #expertise c2) andalso
    (#friendliness c1 = #friendliness c2)

fun compare dist ((c1, c2): cinfo * cinfo) =
    (cwithin dist (#patience c1, #patience c2)) andthen
    (cwithin dist (#confusion c1, #confusion c2)) andthen
    (Int.compare (#boldness c1, #boldness c2)) andthen
    (Int.compare (#expertise c1, #expertise c2)) andthen
    (Int.compare (#friendliness c1, #friendliness c2))

fun init boldness expertise friendliness =
    { patience = Real.fromInt friendliness,
      confusion = 0.0,
      boldness = boldness,
      expertise = expertise,
      friendliness = friendliness }

fun updatep ({patience, confusion, boldness, expertise, friendliness} : cinfo)
            f =
    { patience = f patience,
      confusion = confusion,
      boldness = boldness,
      expertise = expertise,
      friendliness = friendliness }

fun updatec ({patience, confusion, boldness, expertise, friendliness} : cinfo)
            f =
    { patience = patience,
      confusion = f confusion,
      boldness = boldness,
      expertise = expertise,
      friendliness = friendliness }

fun update c pf cf =
    updatep (updatec c cf) pf

fun add (motivation, technical, related)
        (c as {boldness, expertise, friendliness, ...} : cinfo) =
    let val b = Real.fromInt boldness
        val e = Real.fromInt expertise
        val f = Real.fromInt friendliness
        val m = motivation
        val t = technical
        val r = related
    in
        updatec c (fn v =>
                      v + t * (100.0 - e) * 0.1
                      - m * f * 0.05
                      - r * b * 0.05)
    end

fun pause t (c as {boldness, expertise, friendliness, ...} : cinfo) =
    let val b = Real.fromInt boldness
        val e = Real.fromInt expertise
        val f = Real.fromInt friendliness
    in
        update c (fn p => p - t * (100.0 - f) * 0.05)
               (fn c => c - t * e * 0.1)
    end

fun drink (c as {boldness, expertise, friendliness, ...} : cinfo) =
    let val b = Real.fromInt boldness
        val e = Real.fromInt expertise
        val f = Real.fromInt friendliness
    in
        update c (fn p => p - (100.0 - f) * 0.01)
               (fn c => c - e * 0.01)
    end

fun jump (c as {boldness, expertise, friendliness, ...} : cinfo) =
    let val b = Real.fromInt boldness
        val e = Real.fromInt expertise
        val f = Real.fromInt friendliness
    in
        updatec c (fn c => c + 0.02 * ((100.0 - e) + (100.0 - f)))
    end

fun delay (c as {boldness, expertise, friendliness, ...} : cinfo) =
    let val b = Real.fromInt boldness
        val e = Real.fromInt expertise
        val f = Real.fromInt friendliness
    in
        update c (fn p => p - 0.02 * (b + (100.0 - f)))
               (fn c => c + 0.01 * (100.0 - e))
    end

fun dodge (c as {boldness, expertise, friendliness, ...} : cinfo) =
    let val b = Real.fromInt boldness
        val e = Real.fromInt expertise
        val f = Real.fromInt friendliness
    in
        updatep c (fn p => p - 10.0 (* 0.1 *) * (b + (100.0 - f)))
    end

fun remind (c as {boldness, expertise, friendliness, ...} : cinfo) =
    let val b = Real.fromInt boldness
        val e = Real.fromInt expertise
        val f = Real.fromInt friendliness
    in
        updatep c (fn p => p - 0.05 * (b + (100.0 - f)))
    end

fun didntanswer amasker (c as {boldness, expertise, friendliness, ...} : cinfo)
    =
    let val b = Real.fromInt boldness
        val e = Real.fromInt expertise
        val f = Real.fromInt friendliness
    in
        updatep c (fn p => p - 10.0 (* 0.05 *) * (b + (100.0 - f)) * (if amasker then 2.0
                                                           else 1.0))
    end

fun time currtime dt c =
    updatep c (fn p => if GTime.toMinutes currtime > 45.0 then
                           p - dt
                       else
                           p - dt)

end
