structure Pal:
sig
  val longest: char Seq.t -> int * int
end =
struct

  structure AS = ArraySlice

  val p = 1045678717: Int64.int
  val base = 500000000: Int64.int
  val length = Seq.length
  fun sub (str, i) = Seq.nth str i
  fun toStr str = CharVector.tabulate (length str, (fn i => sub (str, i)))

  fun check str =
    let
      val n = length str
      fun fromChar c = Int64.fromInt (Char.ord c)
      fun modp v = v mod p
      fun mul (a, b) = modp (Int64.* (a, b))
      fun add (a, b) = modp (Int64.+ (a, b))
      fun charMul (c, b) = mul (fromChar c, b)

      (* val (basePowers, _) = Seq.scan mul 1 (Seq.tabulate (fn _ => base) n) *)
      val basePowers = AS.full (SeqBasis.scan 10000 mul 1 (0, n) (fn _ => base))

      fun genHash getChar =
        let
          val P = AS.full (SeqBasis.scan 10000 add 0 (0, n)
            (fn i => charMul (getChar i, Seq.nth basePowers i)))
          (* val (P, total) = Seq.scan add 0 (Seq.zipWith charMul (str, basePowers)) *)
          fun H (i, j) =
            let
              val last = Seq.nth P j
              (* val last = if (j = n) then total else Seq.nth P j *)
              val first = Seq.nth P i
              val offset = Seq.nth basePowers (n-i-1)
              open Int64
            in
              modp ((last - first) * offset)
            end
        in
          H
        end

      val forwadHash = genHash (Seq.nth str)
      val backHash = genHash (fn i => Seq.nth str (n-i-1))

      fun perhaps (i, j) = (forwadHash(i, j) = backHash(n-j, n-i))
    in
      perhaps
    end

  (* Verifies that str[i:j] is a palindrome. *)
  fun verify str (i, j) =
    SeqBasis.reduce 10000 (fn (a, b) => a andalso b) true (i, i + (j-i) div 2)
      (fn k => Seq.nth str k = Seq.nth str (i+(j-k-1)))

  fun binarySearch check =
    let
      fun bs (i, j) =
        if j - i = 1 then i
        else
          let
            val mid = (i+j) div 2
          in
            if check mid then bs (mid, j)
            else bs (i, mid)
          end
      fun double i = if check (2*i) then double (2*i) else bs (i, 2*i)
    in
      if check 1 then double 1 else 0
    end

  (* Generates a polynomial hash of the string and reversed string. Then for
   * each index in the string, binary search on the longest palindrome whose
   * middle is that index.
   *)
  fun longest str =
    let
      val n = length str
      (* val isPalinrome = Util.printTime "build" (fn () => check str) *)
      val isPalinrome = check str

      fun maxval ((ia, la), (ib, lb)) = if la > lb then (ia, la) else (ib, lb)

      (* fun getMax f = Seq.reduce maxval (0, 0) (Seq.tabulate f n) *)

      fun getMax f = SeqBasis.reduce 1000 maxval (0, 0) (0, n) f

      fun checkOdd i j =
        (i - j >= 0) andalso (i + j < n) andalso isPalinrome (i - j, i + j + 1)
      val (io, lo) = getMax (fn i => (i, binarySearch (checkOdd i)))

      fun checkEven i j =
        (i - j + 1 >= 0) andalso (i + j < n)
        andalso isPalinrome(i - j + 1, i + j + 1)
      val (ie, le) = getMax (fn i => (i, binarySearch (checkEven i)))

      val (i, l) = if le > lo then (ie-le+1, 2*le) else (io-lo, 2*lo+1)
      val _ = if not(verify str (i, i + l)) then print("Failed!\n")
              else () (* print(toStr(Seq.subseq str (i,l)) ^ "\n") *)
    in
      (i, l)
    end

end
