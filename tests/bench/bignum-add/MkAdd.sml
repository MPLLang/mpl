functor MkAdd (Seq: SEQUENCE) =
struct

  structure ASeq = ArraySequence

  type byte = Word8.word
  type bignum = byte ASeq.t

  fun nth' s i =
    if i < Seq.length s then Seq.nth s i else (0w0: Word8.word)

  fun add (x, y) =
    let
      val x = Seq.fromArraySeq x
      val y = Seq.fromArraySeq y

      val maxlen = Int.max (Seq.length x, Seq.length y)
      val sums = Seq.tabulate (fn i => Word8.+ (nth' x i, nth' y i)) (maxlen+1)

      fun propagate (a, b) =
        if b = 0w127 then a else b
      val (carries, _) = Seq.scan propagate 0w0 sums

      fun f (carry, sum) =
        Word8.andb (Word8.+ (Word8.>> (carry, 0w7), sum), 0wx7F)

      val result =
        Seq.force (Seq.zipWith f (carries, sums))

      val r = Seq.toArraySeq result
    in
      (* [r] might have a trailing 0. Cut it off. *)
      if ASeq.length r = 0 orelse (ASeq.nth r (ASeq.length r - 1) > 0w0) then
        r
      else
        ASeq.take r (ASeq.length r - 1)
    end

end
