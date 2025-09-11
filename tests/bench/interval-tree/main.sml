fun randRange i j =
    i + Word64.toInt (Word64.mod (Util.hash64 (Word64.fromInt i), Word64.fromInt (j - i)))


fun uniform_input n w shuffle =
  let
    fun g i = (randRange 1 w,  i)
    val pairs = Seq.tabulate (fn i => g i) n
  in
    pairs
  end


fun eval_build_im n =
let
  val max_size = 2147483647
  val ilint = uniform_input n max_size false
  val v = Seq.map (fn (i, j) => (i,  (randRange (LargeInt.fromInt i) max_size))) ilint
  val t0 = Time.now ()
  val i = IntervalMap.interval_map v n
  val t1 = Time.now ()
in
  (t0, t1, i)
end

fun eval_queries_im im q =
  let
    val max_size = 2147483647
    val queries = Seq.map (fn i => (#1 i)) (uniform_input q max_size false)
    val t0 = Time.now()
    val r = Seq.map (IntervalMap.stab im) queries
    val t1 = Time.now()
  in
    (t0, t1, r)
  end

fun eval_multi_insert_im im n =
   let
    val max_size = 2147483647
    val ilint = uniform_input n max_size false
    val v = Seq.map (fn (i, j) => (i,  (randRange (LargeInt.fromInt i) max_size))) ilint
    val t0 = Time.now ()
    val i = IntervalMap.multi_insert im v
    val t1 = Time.now ()
  in
    (t0, t1, i)
  end




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

val query_size = CommandLineArgs.parseInt "q" 100000000
val size = CommandLineArgs.parseInt "n" 100000000
val rep = CommandLineArgs.parseInt "repeat" 1

val diff =
          if query_size = 0 then
            run_rounds (fn _ => eval_build_im size) rep
          else
            let
              val c = eval_build_im size
              val curr = eval_queries_im (#3 c)
            in
              run_rounds (fn _ => curr query_size) rep
            end

val _ = print ("total " ^ Time.fmt 4 diff ^ "s\n")
val avg = Time.toReal diff / (Real.fromInt rep)
val _ = print ("average " ^ Real.fmt (StringCvt.FIX (SOME 4)) avg ^ "s\n")


