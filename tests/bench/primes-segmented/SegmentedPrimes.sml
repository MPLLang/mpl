functor SegmentedPrimes
  (I:
   sig
     type t
     val from_int: int -> t
     val to_int: t -> int
   end):
sig
  val primes: int -> I.t Seq.t
  val primes_with_params: {block_size_factor: real, report_times: bool}
                          -> int
                          -> I.t Seq.t
end =
struct

  (* The block size used in the algorithm is approximately
   *   sqrt(n)*block_size_factor
   *
   * Increasing block_size_factor will use larger blocks, which has all of the
   * following effects on performance:
   *   (1) decreased theoretical work
   *   (2) less data locality (?)
   *   (3) less parallelism
   *)
  fun primes_with_params (params as {block_size_factor, report_times}) n :
    I.t Seq.t =
    if n < 2 then
      Seq.empty ()
    else
      let
        val sqrt_n = Real.floor (Math.sqrt (Real.fromInt n))

        val sqrt_primes = primes_with_params params sqrt_n

        (* Split the range [2,n+1) into blocks *)
        val block_size = Real.ceil (Real.fromInt sqrt_n * block_size_factor)
        val block_size = Int.max (block_size, 1000)
        val num_blocks = Util.ceilDiv ((n + 1) - 2) block_size

        val (block_results, tm) = Util.getTime (fn _ =>
          SeqBasis.reduce 1 TreeSeq.append (TreeSeq.empty ()) (0, num_blocks)
            (fn b =>
               let
                 val lo = 2 + b * block_size
                 val hi = Int.min (lo + block_size, n + 1)

                 val flags = Array.array (hi - lo, 0w1 : Word8.word)
                 fun unmark i =
                   Array.update (flags, i - lo, 0w0)

                 fun loop i =
                   if i >= Seq.length sqrt_primes then
                     ()
                   else if 2 * I.to_int (Seq.nth sqrt_primes i) >= hi then
                     ()
                   else
                     let
                       val p = I.to_int (Seq.nth sqrt_primes i)
                       val lom = Int.max (2, Util.ceilDiv lo p)
                       val him = Util.ceilDiv hi p
                     in
                       Util.for (lom, him) (fn m => unmark (m * p));
                       loop (i + 1)
                     end

                 val _ = loop 0

                 val numPrimes = Util.loop (0, hi - lo) 0 (fn (count, i) =>
                   if Array.sub (flags, i) = 0w0 then count else count + 1)

                 val output = ForkJoin.alloc numPrimes

                 val _ = Util.loop (lo, hi) 0 (fn (outi, i) =>
                   if Array.sub (flags, i - lo) = 0w0 then outi
                   else (Array.update (output, outi, I.from_int i); outi + 1))
               in
                 TreeSeq.from_array_seq (ArraySlice.full output)
               end))

        val _ =
          if not report_times then
            ()
          else
            print
              ("sieve   (n = " ^ Int.toString n ^ "): " ^ Time.fmt 4 tm ^ "s\n")

        val (result, tm) = Util.getTime (fn _ =>
          TreeSeq.to_array_seq block_results)

        val _ =
          if not report_times then
            ()
          else
            print
              ("flatten (n = " ^ Int.toString n ^ "): " ^ Time.fmt 4 tm ^ "s\n")
      in
        result
      end


  fun primes n =
    primes_with_params {block_size_factor = 8.0, report_times = false} n

end
