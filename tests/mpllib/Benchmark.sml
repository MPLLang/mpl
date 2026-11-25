structure Benchmark =
struct

  fun getTimes msg n f =
    let
      fun loop tms n =
        let
          val (result, tm) = Util.getTime f
        in
          print (msg ^ " " ^ Time.fmt 4 tm ^ "s\n");

          if n <= 1 then (result, List.rev (tm :: tms))
          else loop (tm :: tms) (n - 1)
        end
    in
      loop [] n
    end

  fun run msg f =
    let
      val warmup = Time.fromReal (CommandLineArgs.parseReal "warmup" 0.0)
      val rep = CommandLineArgs.parseInt "repeat" 1
      val _ = if rep >= 1 then () else Util.die "-repeat N must be at least 1"

      val _ = print ("warmup " ^ Time.fmt 4 warmup ^ "\n")
      val _ = print ("repeat " ^ Int.toString rep ^ "\n")

      fun warmupLoop startTime =
        if Time.>= (Time.- (Time.now (), startTime), warmup) then
          () (* warmup done! *)
        else
          let val (_, tm) = Util.getTime f
          in print ("warmup_run " ^ Time.fmt 4 tm ^ "s\n"); warmupLoop startTime
          end

      val _ =
        if Time.<= (warmup, Time.zeroTime) then
          ()
        else
          ( print ("====== WARMUP ======\n" ^ msg ^ "\n")
          ; warmupLoop (Time.now ())
          ; print ("==== END WARMUP ====\n")
          )

      val _ = print (msg ^ "\n")
      val s0 = RuntimeStats.get ()
      val t0 = Time.now ()
      val (result, tms) = getTimes "time" rep f
      val t1 = Time.now ()
      val s1 = RuntimeStats.get ()
      val endToEnd = Time.- (t1, t0)

      fun stdev rtms avg =
        let
          val SS = List.foldr (fn (a, b) => (a - avg) * (a - avg) + b) 0.0 rtms
          val sample = Real.fromInt (List.length rtms - 1)
        in
          Math.sqrt (SS / sample)
        end

      val rtms = List.map Time.toReal tms
      val total = List.foldl Time.+ Time.zeroTime tms
      val avg = Time.toReal total / (Real.fromInt rep)
      val std = if rep > 1 then stdev rtms avg else 0.0
      val tmax = Time.toReal
        (List.foldl (fn (a, M) => if Time.< (a, M) then M else a) (List.hd tms)
           (List.tl tms))
      val tmin = Time.toReal
        (List.foldl (fn (a, m) => if Time.< (a, m) then a else m) (List.hd tms)
           (List.tl tms))
    in
      print "\n";
      print ("average " ^ Real.fmt (StringCvt.FIX (SOME 4)) avg ^ "s\n");
      print ("minimum " ^ Real.fmt (StringCvt.FIX (SOME 4)) tmin ^ "s\n");
      print ("maximum " ^ Real.fmt (StringCvt.FIX (SOME 4)) tmax ^ "s\n");
      print ("std dev " ^ Real.fmt (StringCvt.FIX (SOME 4)) std ^ "s\n");
      print ("total   " ^ Time.fmt 4 total ^ "s\n");
      print ("end-to-end " ^ Time.fmt 4 endToEnd ^ "s\n");
      RuntimeStats.benchReport {before = s0, after = s1};
      result
    end

end
