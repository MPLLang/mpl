(* do f(i) sequentially for each lo <= i < hi *)
fun for (lo, hi) f =
  if lo >= hi then () else (f lo; for (lo+1, hi) f)

(* do f(i) in parallel for each lo <= i < hi
 * control granularity with `grain` *)
fun parfor grain (lo, hi) f =
  if hi - lo <= grain then
    for (lo, hi) f
  else
    let
      val mid = lo + (hi - lo) div 2
    in
      ForkJoin.fork (fn _ => parfor grain (lo, mid) f,
                     fn _ => parfor grain (mid, hi) f);
      ()
    end

(* build an array in parallel with elements f(i) for each 0 <= i < n *)
fun tabulate (n, f) =
  let
    val arr = ForkJoin.alloc n
  in
    parfor 10000 (0, n) (fn i => Array.update (arr, i, f i));
    arr
  end

val n = CommandLineArgs.parseInt "N" (1000 * 1000 * 1000)
val _ = print ("tabulate " ^ Int.toString n ^ " random characters\n")

fun wfi x = Word64.fromInt x
fun wti u = Word64.toInt u
fun genCharacter i =
  Char.chr (wti (Word64.mod (Util.hash64 (wfi i), wfi (Char.maxOrd+1))))

val t0 = Time.now ()
val result = tabulate (n, genCharacter)
val t1 = Time.now ()

val _ = print ("done\n")
val _ = print (Time.fmt 4 (Time.- (t1, t0)) ^ " s\n")
