structure MainUtils =
struct

  val repeat = ref 1
  val sizes = ref [100]
  val check = ref false
  val explicitGC = ref false
  val printResult = ref false
  val seqCutoffs = ref [35]
  val parCutoffs = ref [50]

  exception Argument of string

  fun setOption r x = r := x

  fun processIntArg p name f nil = raise Argument ("missing value for " ^ name)
    | processIntArg p name f (arg::rest) =
    (case Int.fromString arg
       of NONE => raise Argument ("invalid option for " ^ name)
        | SOME s => (f s; p rest))

  fun processIntListArg p name f nil = raise Argument ("missing value for " ^ name)
    | processIntListArg p name f (arg::rest) =
    let val ts = String.tokens (fn c => c = #",") arg
    in
      case ts
        of nil => raise Argument ("invalid option for " ^ name)
         | ts => f (map (valOf o Int.fromString) ts);
      p rest
    end
     handle Option => raise Argument ("invalid option for " ^ name)

  fun processStringArg p name f nil = raise Argument ("missing value for " ^ name)
    | processStringArg p name f (arg::rest) = (f arg; p rest)

  fun processStringListArg p name f nil = raise Argument ("missing value for " ^ name)
    | processStringListArg p name f (arg::rest) =
        (f (String.tokens (fn c => c = #",") arg); p rest)

  fun processArgs p nil = ()
    | processArgs p (arg::rest) =
    if arg = "--seed" then
      processIntArg p "seed" Random.setSeed rest

    else if arg = "--sizes" then
      processIntListArg p "sizes" (setOption sizes) rest

    else if arg = "--parcuts" then
      processIntListArg p "parcuts" (setOption parCutoffs) rest

    else if arg = "--seqcuts" then
      processIntListArg p "seqcuts" (setOption seqCutoffs) rest

    else if arg = "--repeat" then
      processIntArg p "repeat" (setOption repeat) rest

    else if arg = "--explicitgc" then
      (explicitGC := true; p rest)

    else if arg = "--check" then
      (check := true; p rest)
    else if arg = "--no-check" then
      (check := false; p rest)

    else if arg = "--print" then
      (printResult := true; p rest)

    else raise Argument ("unknown option " ^ arg)

  fun table1 f l = app f l
  fun table2 f l m = app (fn x => table1 (f x) m) l
  fun table3 f l m n = app (fn x => table2 (f x) m n) l
  fun table4 f l m n p = app (fn x => table3 (f x) m n p) l
  fun table5 f l m n p q = app (fn x => table4 (f x) m n p q) l

  fun repeatTest name build test post =
      let
        fun loop _ =
            let
              val input = build ()

              val () = MLton.Parallel.Basic.resetStatistics ()
              val () = MLton.GC.collect ()

              val () = print ("#Starting " ^ name ^ "... ")

              val gctimeatstart = MLton.Parallel.Basic.gcTime ()
              val start = Time.now ()
              val result = test input
              val gctimeatend = MLton.Parallel.Basic.gcTime ()
              val time = Time.- (Time.now (), start)
              val space =
                  let
                    val space = MLton.Parallel.Basic.maxBytesLive ()
                  in
                    if space = 0w0 then
                      (MLton.GC.collect ();
                       MLton.Parallel.Basic.maxBytesLive ())
                    else space
                  end
              val gctime = gctimeatend - gctimeatstart
              val successfulSteals = MLton.Parallel.Basic.successfulSteals ()
              val failedSteals = MLton.Parallel.Basic.failedSteals ()
              val suspends = MLton.Parallel.Basic.suspends ()

              val () = post input result

              val () = print (concat ["#", name, " : ",
                                      Word64.fmt StringCvt.DEC space, " max bytes ",
                                      (LargeInt.toString o Time.toMilliseconds) time, " ms (",
                                      Word64.fmt StringCvt.DEC gctime, " ms in GC) ",
                                      Int.toString successfulSteals, " successful steals (",
                                      Int.toString failedSteals, " failed) ",
                                      Int.toString suspends, " suspends ",
                                      "\n"])
            in
              (space, time, gctime, successfulSteals, failedSteals, suspends)
            end

        fun averageDropMinMax l =
            let
              val l = ListUtil.sort (fn (x, y) => if x = y then EQUAL else if x > y then GREATER else LESS) l
            in
              if length l < 8 then (foldl op+ 0 l) div (length l)
              else
                (foldl op+ 0 (List.take (List.drop (l, 2), length l - 4))) div (length l - 4)
            end

        (* take the max for space, average for time *)
        val (space, time, successfulSteals, failedSteals, suspends) =
            foldl (fn ((s, t, gc, succ, fail, susp), (ms, tt, ss, fs, su)) =>
                      let
                        val t = Time.- (t, (Time.fromMilliseconds (Word64.toLargeInt gc)))
                      in
                        (Word64.max (s, ms), Time.+ (t, tt),
                         succ :: ss, fail :: fs, susp :: su)
                      end)
                  (0w0: Word64.word, Time.zeroTime, nil, nil, nil)
                  (List.tabulate (!repeat, loop))
      in
        { space = space,
          time = LargeInt.div (Time.toMilliseconds time, LargeInt.fromInt (!repeat)),
          successfulSteals = averageDropMinMax successfulSteals,
          failedSteals = averageDropMinMax failedSteals,
          suspends = averageDropMinMax suspends}
      end
end
