structure UsefulRandom =
struct

exception Random of string

structure R = MLton.Random
structure W = Word

val maxWord = W.notb 0w0
val _ = print ((Word.toString maxWord) ^ "\n")

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
    let val diff = W.fromInt (b - a + 1)
        val max = W.* (diff, W.div (maxWord, diff))
        val r = randFilter (fn w => W.< (w, max))
        val wmod = W.mod (r, diff)
    in
        W.toInt wmod
    end

fun rand01 () =
    let val r = randWord ()
        val l = W.toLargeInt r
        val f = Real.fromLargeInt l
        val maxf = Real.fromLargeInt (W.toLargeInt maxWord)
    in
        Real./ (f, maxf)
    end

fun rand01ex () =
    let val r = randFilter (fn w => W.> (w, 0w0))
        val l = W.toLargeInt r
        val f = Real.fromLargeInt l
        val maxf = Real.fromLargeInt (W.toLargeInt maxWord)
    in
        Real./ (f, maxf)
    end

end
