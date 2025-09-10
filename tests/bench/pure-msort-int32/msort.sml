structure CLA = CommandLineArgs

val n = CLA.parseInt "N" (100 * 1000 * 1000)
val quicksortGrain = CLA.parseInt "quicksort" 1024
val grain = CLA.parseInt "grain" 1024
val _ = print ("N " ^ Int.toString n ^ "\n")

val _ = print ("generating " ^ Int.toString n ^ " random integers\n")

val max32 = Word64.fromLargeInt (Int32.toLarge (valOf Int32.maxInt))

fun elem i =
  Int32.fromInt (Word64.toInt (Word64.mod (Util.hash64 (Word64.fromInt i), Word64.fromInt n)))
val input = PureSeq.tabulate elem n

fun sort cmp xs =
  if PureSeq.length xs <= quicksortGrain then
    PureSeq.quicksort cmp xs
  else
    let
      val n = PureSeq.length xs
      val l = PureSeq.take xs (n div 2)
      val r = PureSeq.drop xs (n div 2)
      val (l', r') =
        if n <= grain then
          (sort cmp l, sort cmp r)
        else
          ForkJoin.par (fn _ => sort cmp l, fn _ => sort cmp r)
    in
      PureSeq.merge cmp (l', r')
    end

val result =
  Benchmark.run "running mergesort" (fn _ => sort Int32.compare input)

(* val _ = print ("result " ^ Util.summarizeArraySlice 8 Int.toString result ^ "\n") *)

