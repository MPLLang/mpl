structure UsefulRandom =
struct

exception Random of string

structure MT = MersenneTwister
structure W = LargeWord
structure A = Array

val states = A.tabulate (MLton.Parallel.numberOfProcessors,
                         MT.fromInt)

val maxWord = W.notb 0w0
val maxInt = case Int.maxInt of
                 SOME i => i - 1
               | NONE => 2147483646
val maxIntf = Real.fromInt maxInt

fun seed () = ()
fun useed () = ()

fun randWord () =
    let val p = MLton.Parallel.processorNumber ()
        val rand = A.sub (states, p)
        val (rw, rand') = MT.randomWord rand
    in
        A.update (states, p, rand');
        rw
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
    let val r = randInt (0, maxInt)
        val f = Real.fromInt r
    in
        Real./ (f, maxIntf)
    end

fun rand01ex () =
    let val r = randInt (1, maxInt)
        val f = Real.fromInt r
    in
        Real./ (f, maxIntf)
    end

end
