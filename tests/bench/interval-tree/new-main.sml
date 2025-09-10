structure CLA = CommandLineArgs

val q = CLA.parseInt "q" 100
val n = CLA.parseInt "n" 1000000

val _ = print ("n " ^ Int.toString n ^ "\n")
val _ = print ("q " ^ Int.toString q ^ "\n")

val max_size = 1000000000
(* val gap_size = 100 *)

fun randRange i j seed =
  i + Word64.toInt
        (Word64.mod (Util.hash64 (Word64.fromInt seed), Word64.fromInt (j - i)))

(*
fun randSeg seed =
  let
    val p = gap_size * (randRange 1 (max_size div gap_size) seed)
    val hi = Int.min (p + gap_size, max_size)
  in
    (p, randRange p hi (seed+1))
  end
*)

fun randSeg seed =
  let
    val p = randRange 1 max_size seed
    val space = max_size - p
    val hi = p + 1 + space div 100
  in
    (p, randRange p hi (seed+1))
  end

(* fun query seed =
  IntervalMap.stab tree (randRange 1 max_size seed) *)

fun query tree seed =
  IntervalMap.size (IntervalMap.report_all tree (randRange 1 max_size seed))

fun bench () =
  let
    val (tree, tm) = Util.getTime (fn _ =>
      IntervalMap.interval_map (Seq.tabulate (fn i => randSeg (2*i)) n) n)
    val _ = print ("generated tree in " ^ Time.fmt 4 tm ^ "s\n")
  in
    ArraySlice.full (SeqBasis.tabulate 1 (0, q) (fn i => query tree (2*n + i)))
  end

val result = Benchmark.run "generating and stabbing intervals..." bench

(* val numHits = Seq.reduce op+ 0 (Seq.map (fn true => 1 | _ => 0) result)
val _ = print ("hits " ^ Int.toString numHits ^ "\n")

val hitrate = Real.round (100.0 * (Real.fromInt numHits / Real.fromInt q))
val _ = print ("hitrate " ^ Int.toString hitrate ^ "%\n") *)

val numHits = Seq.reduce op+ 0 result
val minHits = Seq.reduce Int.min (valOf Int.maxInt) result
val maxHits = Seq.reduce Int.max 0 result
val avgHits = Real.round (Real.fromInt numHits / Real.fromInt q)
val _ = print ("hits " ^ Int.toString numHits ^ "\n")
val _ = print ("min " ^ Int.toString minHits ^ "\n")
val _ = print ("avg " ^ Int.toString avgHits ^ "\n")
val _ = print ("max " ^ Int.toString maxHits ^ "\n")
