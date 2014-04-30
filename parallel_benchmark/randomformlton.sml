structure Random =
struct

  val setSeed = MLton.Random.srand o Word.fromInt

  fun randomInt i = Word.toIntX (MLton.Random.rand ()) mod i

  fun randomReal x = Real.rem (MLton.Real.fromWord (MLton.Random.rand ()), x)

  fun randomBool () = randomInt 2 = 0

end
