structure UsefulRandom =
struct

exception Random of string

structure R = MLton.Random
structure W = Word16
structure A = Array

val states = A.tabulate (MLton.Parallel.numberOfProcessors,
                         fn i => W.fromInt (i * 2 + 1))

val p1 = 0w14057
val p2 = 0w33353
val maxWord = W.notb 0w0

fun seed () = ()

fun useed () = ()

fun randByte p =
    let val s = A.sub (states, p)
        val s' = W.mod(W.+ (s, p2), p1)
    in
        A.update (states, p, s');
        W.andb (s', 0w255)
    end

fun randWord () =
    let val p = MLton.Parallel.processorNumber ()
        val b1 = randByte p
        val b2 = randByte p
        val b3 = randByte p
        val b4 = randByte p
    in
        (* W.+ (W.<< (b1, 0w24),
             W.+ (W.<< (b2, 0w16), *)
                  W.+ (W.<< (b3, 0w8),
                       b4) (* )) *)
    end

fun randFilterCount n f =
    let val r = randWord ()
        val _ = if n > 10 then print ((Int.toString n) ^ "\n") else ()
    in
        if f r then
            r
        else
            randFilterCount (n + 1) f
    end

val randFilter = randFilterCount 0

fun randInt (a, b) =
    let (* val _ = print "randInt\n" *)
        val diff = W.fromInt (b - a + 1)
        val max = W.* (diff, W.div (maxWord, diff))
        val r = randFilter (fn w => W.< (w, max))
        val wmod = W.mod (r, diff)
        val res = W.toInt wmod
    in
        res
(*         before print "end randInt\n" *)
(*         before (print ((Int.toString res) ^ "\n")) *)
    end

fun rand01 () =
    let val r = randWord ()
        val l = W.toInt r
        val f = Real.fromInt l
        val maxf = Real.fromLargeInt (W.toLargeInt maxWord)
    in
        Real./ (f, maxf)
    end

fun rand01ex () =
    let val r = randFilter (fn w => W.> (w, 0w0))
        val l = W.toInt r
        val f = Real.fromInt l
        val maxf = Real.fromLargeInt (W.toLargeInt maxWord)
    in
        Real./ (f, maxf)
    end

end
