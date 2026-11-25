structure Add =
struct
  structure Seq = ArraySequence

  type byte = Word8.word
  type bignum = byte Seq.t

  fun init (b1, b2) =
    Word8.+ (b1, b2)

  fun copy (a, b) =
    if b = 0w127 then a else b


  fun add (x, y) =
    let
      val nx = Seq.length x
      val ny = Seq.length y
      val n = Int.max (nx, ny)

      fun nthx i = if i < nx then Seq.nth x i else 0w0
      fun nthy i = if i < ny then Seq.nth y i else 0w0

      val blockSize = 10000
      val numBlocks = 1 + ((n-1) div blockSize)

      val blockCarries =
        SeqBasis.tabulate 1 (0, numBlocks) (fn blockIdx =>
          let
            val lo = blockIdx * blockSize
            val hi = Int.min (lo + blockSize, n)
            fun loop acc i =
              if i >= hi then
                acc
              else
                loop (copy (acc, init (nthx i, nthy i))) (i+1)
          in
            loop 0w0 lo
          end)

      val blockPartials =
        SeqBasis.scan 5000 copy 0w0 (0, numBlocks)
        (fn i => Array.sub (blockCarries, i))

      val lastCarry = Array.sub (blockPartials, numBlocks)

      val result = ForkJoin.alloc (n+1)

      val _ =
        ForkJoin.parfor 1 (0, numBlocks) (fn blockIdx =>
          let
            val lo = blockIdx * blockSize
            val hi = Int.min (lo + blockSize, n)

            fun loop acc i =
              if i >= hi then
                ()
              else
                let
                  val sum = init (nthx i, nthy i)
                  val acc' = copy (acc, sum)
                  val thisByte =
                    Word8.andb (Word8.+ (Word8.>> (acc, 0w7), sum), 0wx7F)
                in
                  Array.update (result, i, thisByte);
                  loop acc' (i+1)
                end
          in
            loop (Array.sub (blockPartials, blockIdx)) lo
          end)

    in
      if lastCarry > 0w127 then
        (Array.update (result, n, 0w1); ArraySlice.full result)
      else
        (ArraySlice.slice (result, 0, SOME n))
    end
end
