structure SequentialAdd =
struct
  structure A = Array
  structure AS = ArraySlice
  structure Seq = ArraySequence

  (* radix 128 *)
  type byte = Word8.word
  type bignum = byte Seq.t

  fun addWithCarry3 (c, b1, b2) =
    let
      val x = Word8.+ (b1, Word8.+ (b2, c))
    in
      {result = Word8.andb (x, 0wx7F), carry = Word8.>> (x, 0w7)}
    end

  fun addWithCarry2 (b1, b2) =
    addWithCarry3 (0w0, b1, b2)

  fun add (s1, s2) =
    let
      val n1 = Seq.length s1
      val n2 = Seq.length s2
      val n = Int.max (n1, n2)

      val r = ForkJoin.alloc (1 + n)

      fun finish1 i carry =
        if i = n1 then
          (A.update (r, i, carry); carry)
        else
          let
            val {result, carry=carry'} = addWithCarry2 (Seq.nth s1 i, carry)
          in
            A.update (r, i, result);
            finish1 (i+1) carry'
          end

      fun finish2 i carry =
        if i = n2 then
          (A.update (r, i, carry); carry)
        else
          let
            val {result, carry=carry'} = addWithCarry2 (Seq.nth s2 i, carry)
          in
            A.update (r, i, result);
            finish2 (i+1) carry'
          end

      fun loop i carry =
        if i = n1 then
          finish2 i carry
        else if i = n2 then
          finish1 i carry
        else
          let
            val {result, carry=carry'} =
              addWithCarry3 (Seq.nth s1 i, Seq.nth s2 i, carry)
          in
            A.update (r, i, result);
            loop (i+1) carry'
          end
    in
      (** Run the loop, and inspect the last carry value.
        * If it is 1, then the output is well-formed.
        * If it is 0, we need to trim.
        *)
      case loop 0 0w0 of
        0w0 =>
          AS.slice (r, 0, SOME n)
      | _ =>
          AS.full r
    end

end
