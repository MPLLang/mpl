(* build an array in parallel with elements f(i) for each 0 <= i < n *)
fun tabulate (n, f) =
  let
    val arr = ForkJoin.alloc n
  in
    Util.parfor 10000 (0, n) (fn i => Array.update (arr, i, f i));
    arr
  end

(* generate the ith element with a hash function *)
fun gen seed i = Util.hash64 (Word64.xorb (Word64.fromInt i, seed))

(* ==========================================================================
 * parse command-line arguments and run
 *)

val n = CommandLineArgs.parseInt "N" (1000 * 1000 * 1000)
val seed = CommandLineArgs.parseInt "seed" 0
val _ = print ("tabulate " ^ Int.toString n ^ " pseudo-random 64-bit words\n")
val _ = print ("seed " ^ Int.toString seed ^ "\n")

val seed' = Util.hash64 (Word64.fromInt seed)

val t0 = Time.now ()
val result = tabulate (n, gen seed')
val t1 = Time.now ()

val _ = print ("finished in " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s\n")

fun str x = Word64.fmt StringCvt.HEX x
val _ = print ("result " ^ Util.summarizeArray 3 str result ^ "\n")
