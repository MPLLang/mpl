(* MUCH faster random number generation than DotMix.
 * I wonder how good its randomness is? *)
structure FastHashRand =
struct
  type rand = Word64.word

  val maxWord = 0wxFFFFFFFFFFFFFFFF : Word64.word

  exception FastHashRand

  fun hashWord w =
    let
      open Word64
      infix 2 >> infix 2 << infix 2 xorb infix 2 andb
      val v = w * 0w3935559000370003845 + 0w2691343689449507681
      val v = v xorb (v >> 0w21)
      val v = v xorb (v << 0w37)
      val v = v xorb (v >> 0w4)
      val v = v * 0w4768777513237032717
      val v = v xorb (v << 0w20)
      val v = v xorb (v >> 0w41)
      val v = v xorb (v << 0w5)
    in
      v
    end

  fun fromInt x = hashWord (Word64.fromInt x)

  fun next r = hashWord r

  fun split r = (hashWord r, (hashWord (r+0w1), hashWord (r+0w2)))

  fun biasedBool (h, t) r =
    let
      val scaleFactor = Word64.div (maxWord, Word64.fromInt (h+t))
    in
      Word64.<= (r, Word64.* (Word64.fromInt h, scaleFactor))
    end

  fun split3 _ = raise FastHashRand
  fun splitTab (r, n) =
    (hashWord r, fn i => hashWord (r + Word64.fromInt (i+1)))

  val intp =
    case Int.precision of
      SOME n => n
    | NONE => (print "[ERR] int precision\n"; OS.Process.exit OS.Process.failure)

  val mask = Word64.<< (0w1, Word.fromInt (intp-1))

  fun int r =
    Word64.toIntX (Word64.andb (r, mask) - 0w1)

  fun int r =
    Word64.toIntX (Word64.>> (r, Word.fromInt (64-intp+1)))

  fun boundedInt (a, b) r = a + ((int r) mod (b-a))

  fun bool _ = raise FastHashRand

  fun biasedInt _ _ = raise FastHashRand
  fun real _ = raise FastHashRand
  fun boundedReal _ _ = raise FastHashRand
  fun char _ = raise FastHashRand
  fun boundedChar _ _ = raise FastHashRand
end
