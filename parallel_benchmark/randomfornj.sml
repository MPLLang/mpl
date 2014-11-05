structure Random =
struct

  val seed = ref 0w5678

  fun setSeed i = seed := Word.fromInt i

  fun randomInt i =
    let in
	seed := (Word.andb (!seed * 0w10007 + 0w1367, 0wxEADBEEF));
	(Word.toInt (!seed)) mod i
    end

  fun randomReal x =
      Real.rem (Real.fromInt (randomInt (Real.toInt IEEEReal.TO_NEAREST 
                                                    (x * 1000.0))) / 1000.0, x)

  fun randomBool () = randomInt 2 = 0

end
