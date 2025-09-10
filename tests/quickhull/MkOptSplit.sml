functor MkSplit (Seq: SEQUENCE) :
sig
  type 'a seq = 'a Seq.t
  type 'a aseq = 'a ArraySequence.t

  datatype flag = Left | Right | Throwaway
  val parSplit: 'a seq -> flag seq -> 'a aseq * 'a aseq
end =
struct

  structure A = Array
  structure AS = ArraySlice
  structure ASeq = ArraySequence

  type 'a seq = 'a Seq.t
  type 'a aseq = 'a ASeq.t

  fun inPlace (s: 'a ASeq.t) putLeft putRight =
    let
      val (a, start, n) = AS.base s
      fun item i = Array.sub (a, i)
      fun set i x = Array.update (a, i, x)

      fun growLeft ll lm rm rr =
        if lm >= rm then (ll, rr) else
        let
          val x = item lm
        in
          if putRight x then
            growRight ll lm rm rr
          else if not (putLeft x) then
            growLeft ll (lm+1) rm rr
          else
            (set ll x; growLeft (ll+1) (lm+1) rm rr)
        end

      and growRight ll lm rm rr =
        if lm >= rm then (ll, rr) else
        let
          val x = item (rm-1)
        in
          if putLeft x then
            swapThenContinue ll lm rm rr
          else if not (putRight x) then
            growRight ll lm (rm-1) rr
          else
            (set (rr-1) x; growRight ll lm (rm-1) (rr-1))
        end

      and swapThenContinue ll lm rm rr =
        let
          val tmp = item lm
        in
          set ll (item (rm-1));
          set (rr-1) tmp;
          growLeft (ll+1) (lm+1) (rm-1) (rr-1)
        end

      val (ll, rr) = growLeft start start (start+n) (start+n)
    in
      (ll-start, (start+n)-rr)
    end

  datatype flag = Left | Right | Throwaway

  fun parSplit s flags =
    let
      val n = Seq.length s
      val blockSize = 10000
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
              (* Array.update (counts, b, (cl, cr)) *)
              (Array.update (countl, b, cl); Array.update (countr, b, cr))
            else case Seq.nth flags i of
              Left => loop (cl+1, cr) (i+1)
            | Right => loop (cl, cr+1) (i+1)
            | _ => loop (cl, cr) (i+1)
        in
          loop (0, 0) lo
        end)

      (* val (offsets, (totl, totr)) =
        Seq.scan (fn ((a,b),(c,d)) => (a+c,b+d)) (0,0) (ArraySlice.full counts) *)
      val (offsetsl, totl) = ASeq.scan op+ 0 (ArraySlice.full countl)
      val (offsetsr, totr) = ASeq.scan op+ 0 (ArraySlice.full countr)

      val left = ForkJoin.alloc totl
      val right = ForkJoin.alloc totr

      val _ = ForkJoin.parfor 1 (0, numBlocks) (fn b =>
        let
          val lo = b * blockSize
          val hi = Int.min (lo + blockSize, n)
          (* val (offsetl, offsetr) = Seq.nth offsets b *)
          val offsetl = ASeq.nth offsetsl b
          val offsetr = ASeq.nth offsetsr b
          fun loop (cl, cr) i =
            if i >= hi then () else
            case Seq.nth flags i of
              Left => (Array.update (left, offsetl+cl, Seq.nth s i); loop (cl+1, cr) (i+1))
            | Right => (Array.update (right, offsetr+cr, Seq.nth s i); loop (cl, cr+1) (i+1))
            | _ => loop (cl, cr) (i+1)
        in
          loop (0, 0) lo
        end)
    in
      (ArraySlice.full left, ArraySlice.full right)
    end

end
