structure MersenneTwister :>
sig
  type rand
  val fromInt : int -> rand
  val randomWord : rand -> LargeWord.word * rand
end =
struct
  structure LW = LargeWord
  type state = (int * LW.word Array.array)
  type rand = state

  val arrlen = 624
  val offset = 397

  (* Helper for imperative things *)
  fun loop i f =
    if   i <= 0
    then ()
    else (f (i - 1); loop (i - 1) f)

  (* Create a rand state from an int seed. *)
  local
    val magic : LW.word = 0wx6c078965
    open Array
  in
  fun fromInt (i : int) : rand =
    let
      val mt : LW.word array = array (arrlen, LW.fromInt i)

      (* Generate the state imperatively *)
      fun looper i =
        let
          val prev = sub (mt, i)
          val shifted = LW.>> (prev, 0wx1e)
          val xored = LW.xorb (prev, shifted)
          val muled = LW.* (magic, xored)
          val added = LW.+ (muled, LW.fromInt i)
        in
          update (mt, i+1, added)
        end
        val _ = loop 623 looper
    in
      (0, mt)
    end
  end


  local
    val magic : LW.word = 0wx9908b0df
    open Array
  in
  fun generate orig =
    let
      (* Create a copy of the original state array *)
      val mt : LW.word array = array (arrlen, 0wx0)
      val _ = copy {src = orig, dst = mt, di = 0}

      (* Modify the new state *)
      fun looper i =
        let
          val topbit =
            LW.andb (sub (mt, i), 0wx80000000)
          val lowbits =
            LW.andb (sub (mt, (i + 1) mod arrlen), 0wx7fffffff)
          val y = LW.+ (topbit, lowbits)
          val maybe =
            LW.xorb (sub (mt, (i + offset) mod arrlen), LW.>> (y, 0wx1))
          val new =
            if   LW.mod (y, 0wx2) = 0wx0
            then maybe
            else LW.xorb (maybe, magic)
        in
          update (mt, i, new)
        end
      val _ = loop 624 looper
    in
      mt
    end
  end

  local
    val magic1 : LW.word = 0wx9d2c5680
    val magic2 : LW.word = 0wxefc60000
    open Array
  in
  fun randomWord (ind, orig) =
    let
      val mt = case ind of 0 => generate orig | _ => orig
      val y = sub (mt, ind)

      val y1 = LW.xorb (y, LW.>> (y, 0wxb))
      val y2 = LW.xorb (y1, LW.andb (LW.<< (y1, 0wx7), magic1))
      val y3 = LW.xorb (y2, LW.andb (LW.<< (y2, 0wxf), magic2))
      val y4 = LW.xorb (y3, LW.>> (y3, 0wx12))
    in
      (y4, ((ind + 1) mod arrlen, mt))
    end
  end

end
