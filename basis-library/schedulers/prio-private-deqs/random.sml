structure UsefulRandom =
struct

exception Random of string

structure R = MLton.Random
structure W = Word

val maxWord = W.notb 0w0

fun seed () =
    case R.seed () of
        NONE => raise (Random "seed failed")
      | SOME w => R.srand w

fun useed () =
    case R.useed () of
        NONE => raise (Random "seed failed")
      | SOME w => R.srand w

fun randWord () = R.rand ()

fun randFilter f =
    let val r = randWord ()
    in
        if f r then
            r
        else
            randFilter f
    end

fun randInt (a, b) =
    let val diff = b - a + 1
        val max = diff * (W.mod (maxWord, diff))
        val r = randFilter (fn w => W.< (w, max))
        val wmod = W.mod (r, diff)
    in
        W.toInt wmod
    end

fun rand01 () =
    let val r = randWord ()
        val l = W.toLarge r
        val f = Real.fromLargeInt l
        val maxf = Real.fromLargeInt (W.toLarge maxWord)
    in
        Real./ (f, maxf)
    end

fun rand01ex () =
    let val r = randFilter (fn w => W.> (w, 0w0))
        val l = W.toLarge r
        val f = Real.fromLargeInt l
        val maxf = Real.fromLargeInt (W.toLarge maxWord)
    in
        Real./ (f, maxf)
    end

end
