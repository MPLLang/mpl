structure CLA = CommandLineArgs

val q = CLA.parseInt "q" 10000000
val n = CLA.parseInt "n" 10000000

val max_size = 2147483647

fun randRange i j seed =
  i + Word64.toInt
        (Word64.mod (Util.hash64 (Word64.fromInt seed), Word64.fromInt (j - i)))

fun randPt seed =
  let
    val p = randRange 0 max_size seed
  in
    (randRange 0 max_size seed, randRange 0 max_size (seed+1))
  end

(* copied from PAM: range_utils.h *)
fun generate_points n seed =
  let
    fun rand_coordinate i = randRange 0 max_size i
    val rand_numbers = Seq.tabulate rand_coordinate (3*n)
    val get = Seq.nth rand_numbers
    val points = Seq.tabulate (fn i => ((get i, get (i + n)), get (i + 2*n))) n
  in
    points
  end

val (tree, tm) = Util.getTime (fn _ =>
  RangeTree.build (generate_points n 0) n)
val _ = print ("generated input in " ^ Time.fmt 4 tm ^ "s\n")

fun query i =
  let
    val p1 = randPt (4*i)
    val p2 = randPt (4*i + 2)
  in
    RangeTree.query tree p1 p2
  end

fun bench () = SeqBasis.tabulate 100 (0, q) query
val result = Benchmark.run "querying range tree" bench
val _ = Util.summarizeArray 10 Int.toString result

(*

fun run_rounds f r =
  let
    fun round_rec i diff =
      if i = 0 then diff
      else
        let
          val (t0, t1, _) = f()
          val new_diff = Time.- (t1, t0)
          val _ = print ("round " ^ (Int.toString (r - i + 1)) ^ " in " ^ Time.fmt 4 (new_diff) ^ "s\n")
        in
          round_rec (i - 1) (Time.+ (diff, new_diff))
        end
  in
    round_rec r Time.zeroTime
  end

fun eval_build_range_tree n =
  let
    val points = generate_points n 0
    val t0 = Time.now ()
    val rt = RangeTree.build points n
    val t1 = Time.now ()
  in
    (t0, t1, rt)
  end

fun eval_queries_range_tree rt q =
  let
    val max_size = 2147483647
    val pl = generate_points q 0
    val pr = generate_points q 0
    val t0 = Time.now()
    val r = SeqBasis.tabulate 100 (0, q) (fn i => RangeTree.query rt (#1 (Seq.nth pl i)) (#1 (Seq.nth pr i)))
    val t1 = Time.now()
  in
    (t0, t1, 0)
  end

val query_size = CommandLineArgs.parseInt "q" 10000000
val size = CommandLineArgs.parseInt "n" 10000000
val rep = CommandLineArgs.parseInt "repeat" 1

val diff =
          if query_size = 0 then
            run_rounds (fn _ => eval_build_range_tree size) rep
          else
            let
              val curr = eval_queries_range_tree (#3 (eval_build_range_tree size))
            in
              run_rounds (fn _ => curr query_size) rep
            end

val _ = print ("total " ^ Time.fmt 4 diff ^ "s\n")
val avg = Time.toReal diff / (Real.fromInt rep)
val _ = print ("average " ^ Real.fmt (StringCvt.FIX (SOME 4)) avg ^ "s\n")
*)
