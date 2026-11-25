structure LinearRec =
struct

  structure A = Array
  structure AS = ArraySlice

  fun upd a i x = A.update (a, i, x)
  fun nth a i   = A.sub (a, i)

  val parfor = ForkJoin.parfor
  val par = ForkJoin.par
  val allocate = ForkJoin.alloc

  type elem = real * real

  fun scanMap grain (g: elem * elem -> elem) b (lo, hi) (f : int -> elem) (out: elem -> 'a) =
    if hi - lo <= grain then
      let
        val n = hi - lo
        val result = allocate (n+1)
        fun bump ((j,b),x) = (upd result j (out b); (j+1, g (b, x)))
        val (_, total) = SeqBasis.foldl bump (0, b) (lo, hi) f
      in
        upd result n (out total);
        result
      end
    else
      let
        val n = hi - lo
        val k = grain
        val m = 1 + (n-1) div k (* number of blocks *)
        val sums = SeqBasis.tabulate 1 (0, m) (fn i =>
          let val start = lo + i*k
          in SeqBasis.foldl g b (start, Int.min (start+k, hi)) f
          end)
        val partials = SeqBasis.scan grain g b (0, m) (nth sums)
        val result = allocate (n+1)
      in
        parfor 1 (0, m) (fn i =>
          let
            fun bump ((j,b),x) = (upd result j (out b); (j+1, g (b, x)))
            val start = lo + i*k
          in
            SeqBasis.foldl bump (i*k, nth partials i) (start, Int.min (start+k, hi)) f;
            ()
          end);
        upd result n (out (nth partials m));
        result
      end

  (* ====================================================================== *)

  fun combine ((x1, y1), (x2, y2)) =
    (x1 * x2, y1 * x2 + y2)

  val id = (1.0, 0.0)

  fun linearRec s =
    let
      val result =
        scanMap 5000 combine id (0, Seq.length s) (Seq.nth s) #2
    in
      Seq.drop (AS.full result) 1
    end

end
