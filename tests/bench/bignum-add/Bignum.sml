structure Bignum =
struct

  structure Seq = ArraySequence

  type byte = Word8.word

  (* radix 128 representation *)
  type t = byte Seq.t

  type bignum = t

  fun properlyFormatted x =
    Seq.length x = 0 orelse Seq.nth x (Seq.length x - 1) <> 0w0

  fun fromIntInf (x: IntInf.int): bignum =
    if x < 0 then
      raise Fail "bignums can't be negative"
    else
      let
        fun toList (x: IntInf.int) : byte list =
          if x = 0 then []
          else Word8.fromInt (IntInf.toInt (x mod 128)) :: toList (x div 128)
      in
        Seq.fromList (toList x)
      end

  fun toIntInf (n: bignum): IntInf.int =
    if not (properlyFormatted n) then
      raise Fail "invalid bignum"
    else
      let
        val n' = Seq.map (IntInf.fromInt o Word8.toInt) n
      in
        Seq.iterate (fn (x, d) => 128 * x + d) (0: IntInf.int) (Seq.rev n')
      end

  fun generate n seed =
    let
      fun hash seed = Util.hash32_2 (Word32.fromInt seed)
      fun w32to8 w = Word8.fromInt (Word32.toInt (Word32.andb (w, 0wx7F)))
      fun genByte seed = w32to8 (hash seed)
      fun genNonZeroByte seed =
        w32to8 (Word32.+ (0w1, Word32.mod (hash seed, 0w127)))
    in
      Seq.tabulate (fn i =>
        if i < n-1 then
          genByte (seed+i)
        else
          genNonZeroByte (seed+i))
      n
    end

  fun toString x =
    Seq.toString Word8.toString x

end
