structure Split:
sig
  type 'a dseq
  type 'a seq
  val split: 'a dseq -> bool dseq -> {true: 'a seq, false: 'a seq}
end =
struct

  structure A = Array
  structure AS = ArraySlice

  structure DS = DelayedSeq
  type 'a dseq = 'a DS.t
  type 'a seq = 'a AS.slice

  fun split s flags =
    let
      val n = DS.length s
      val blockSize = 2000
      val numBlocks = 1 + (n-1) div blockSize

      (* the later scan(s) appears to be faster when split into two separate
       * scans, rather than doing a single scan on tuples. *)

      (* val counts = Primitives.alloc numBlocks *)
      val countl = ForkJoin.alloc numBlocks
      val countr = ForkJoin.alloc numBlocks

      val _ = ForkJoin.parfor 1 (0, numBlocks) (fn b =>
        let
          val lo = b * blockSize
          val hi = Int.min (lo + blockSize, n)
          fun loop (cl, cr) i =
            if i >= hi then
              (* A.update (counts, b, (cl, cr)) *)
              (A.update (countl, b, cl); A.update (countr, b, cr))
            else if DS.nth flags i then
              loop (cl+1, cr) (i+1)
            else
              loop (cl, cr+1) (i+1)
        in
          loop (0, 0) lo
        end)

      (* val (offsets, (totl, totr)) =
        Seq.scan (fn ((a,b),(c,d)) => (a+c,b+d)) (0,0) (ArraySlice.full counts) *)
      val (offsetsl, totl) = Seq.scan op+ 0 (AS.full countl)
      val (offsetsr, totr) = Seq.scan op+ 0 (AS.full countr)

      val left = ForkJoin.alloc totl
      val right = ForkJoin.alloc totr

      val _ = ForkJoin.parfor 1 (0, numBlocks) (fn b =>
        let
          val lo = b * blockSize
          val hi = Int.min (lo + blockSize, n)
          (* val (offsetl, offsetr) = Seq.nth offsets b *)
          val offsetl = Seq.nth offsetsl b
          val offsetr = Seq.nth offsetsr b
          fun loop (cl, cr) i =
            if i >= hi then ()
            else if DS.nth flags i then
              (A.update (left, offsetl+cl, DS.nth s i); loop (cl+1, cr) (i+1))
            else
              (A.update (right, offsetr+cr, DS.nth s i); loop (cl, cr+1) (i+1))
        in
          loop (0, 0) lo
        end)
    in
      {true = AS.full left, false = AS.full right}
    end

end
