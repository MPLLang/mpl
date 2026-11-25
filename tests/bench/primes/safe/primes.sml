(* primes: int -> int array
 * generate all primes up to (and including) n *)
fun primes n =
  if n < 2 then ForkJoin.alloc 0 else
  let
    (* all primes up to sqrt(n) *)
    val sqrtPrimes = primes (Real.floor (Math.sqrt (Real.fromInt n)))

    (* allocate array of flags to mark primes. *)
    val flags = ForkJoin.alloc (n+1) : Word8.word array
    fun mark i = Array.update (flags, i, 0w0)
    fun unmark i = Array.update (flags, i, 0w1)
    fun isMarked i = Array.sub (flags, i) = 0w0

    (* initially, mark every number *)
    val _ = ForkJoin.parfor 10000 (0, n+1) mark

    (* unmark every multiple of every prime in sqrtPrimes *)
    val _ =
      ForkJoin.parfor 1 (0, Array.length sqrtPrimes) (fn i =>
        let
          val p = Array.sub (sqrtPrimes, i)
          val numMultiples = n div p - 1
        in
          ForkJoin.parfor 4096 (0, numMultiples) (fn j => unmark ((j+2) * p))
        end)
  in
    (* for every i in 2 <= i <= n, filter those that are still marked *)
    SeqBasis.filter 4096 (2, n+1) (fn i => i) isMarked
  end

(* ==========================================================================
 * parse command-line arguments and run
 *)

val n = CommandLineArgs.parseInt "N" (100 * 1000 * 1000)
val _ = print ("generating primes up to " ^ Int.toString n ^ "\n")

val t0 = Time.now ()
val result = primes n
val t1 = Time.now ()

val _ = print ("finished in " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s\n")

val numPrimes = Array.length result
val _ = print ("number of primes " ^ Int.toString numPrimes ^ "\n")
val _ = print ("result " ^ Util.summarizeArray 8 Int.toString result ^ "\n")
