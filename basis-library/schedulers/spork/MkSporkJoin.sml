functor MkSporkJoin
  (val spork: (unit -> 'a) * (unit -> 'b) * ('a -> 'c) * ('a * 'b -> 'c) -> 'c
   val tryPromoteNow: {youngestOptimization: bool} -> unit
   val noTokens: unit -> bool) :>
sig
  include SPORK_JOIN
  val numSpawnsSoFar: unit -> int
  val numEagerSpawnsSoFar: unit -> int
  val numHeartbeatsSoFar: unit -> int
  val numSkippedHeartbeatsSoFar: unit -> int
  val numStealsSoFar: unit -> int
end =
struct
  fun for (i, j) f =
    if i >= j then () else (f i; for (i + 1, j) f)

  val basicSpork = spork

  fun spork (cont: unit -> 'a, spwn: unit -> 'b,
             seq: 'a -> 'c, sync: 'a * 'b -> 'c) : 'c =
    let
      fun cont' () =
        ( if noTokens () then
            ()
          else
            tryPromoteNow {youngestOptimization = true}
        ; cont ()
        )
    in
      basicSpork (cont', spwn, seq, sync)
    end

  fun par (f: unit -> 'a, g: unit -> 'b) : 'a * 'b =
    let
      fun f' () =
        ( if noTokens () then
            ()
          else
            tryPromoteNow {youngestOptimization = true}
        ; f ()
        )
    in
      basicSpork (f', g, fn a => (a, g ()), fn (a, b) => (a, b))
    end

  val fork = par

  (* ======================================================================= *)

  fun parfor (lo, hi) f =
    if noTokens () then
      parfor_spork_one_body (lo, hi) f
    else
      parfor_split (lo, hi) f

  and parfor_spork_one_body (lo, hi) f =
    if lo >= hi then
      ()
    else
      let
        val lo' = lo+1
        fun body () = f lo

        fun seq () = parfor_spork_one_body (lo', hi) f
        fun spwn () = parfor (lo', hi) f
        fun sync ((), ()) = ()
      in
        spork (body, spwn, seq, sync)
      end

  and parfor_split (lo, hi) f =
    if lo >= hi then
      ()
    else if lo+1 = hi then
      f lo
    else
      let
        val mid = lo + (hi-lo) div 2
        fun left () = parfor (lo, mid) f
        fun right () = parfor (mid, hi) f
      in
        par (left, right);
        ()
      end

  fun parfor' _ (lo, hi) f = parfor (lo, hi) f
  val parfor = parfor'

  (* ======================================================================= *)



  fun pareduce (i: int, j: int, z: 'a, step: int * 'a -> 'a, merge: 'a * 'a -> 'a) : 'a =
      let fun loop (a: 'a, i: int, j: int) : 'a =
              if i >= j then
                a
              else
                spork
                  (fn () => step (i, a),
                   fn () => let val mid = (i + j) div 2
                                fun left_half () = loop (z, i + 1, mid)
                                fun right_half () = loop (z, mid, j)
                                val (al, ar) = par (left_half, right_half)
                            in
                              merge (al, ar)
                            end,
                  fn (a') => loop (a', i + 1, j),
                  merge)
      in
        loop (z, i, j)
      end

  fun pareduce' (i: int, j: int, z: 'a, iter: int -> 'a, merge: 'a * 'a -> 'a) : 'a =
      let fun loop (a: 'a, i: int, j: int) =
              if i >= j then
                a
              else
                spork
                  (fn () => merge (a, iter i),
                   fn () => let val mid = (i + j) div 2
                                fun left_half () = loop (z, i + 1, mid)
                                fun right_half () = loop (z, mid, j)
                                val (al, ar) = par (left_half, right_half)
                            in
                              merge (a, merge (al, ar))
                            end,
                   fn (a') => loop (a', i + 1, j),
                   merge)
      in
        loop (z, i, j)
      end

  fun par (f: unit -> 'a, g: unit -> 'b) : 'a * 'b =
      spork (f, g, fn (a) => (a, g ()), fn (ab) => ab)

  fun parfor'' (i: int, j: int, iter: int -> unit) : unit =
      let (*fun exp_ceil_log n =
              (* Computes 2^ceil(log(n)),
                 i.e. rounds n up to the nearest power of 2 *)
              let fun h m =
                      if m >= n then m else h (m + m)
              in h 1 end
          fun computeMid (start: int, i: int, stop: int) : int =
              let val mid = Int.div (start + stop, 2) in
                if mid < i then
                  computeMid (mid, i, stop)
                else
                  mid
              end
          val j' = i + exp_ceil_log (j - i)*)

          fun computeMid (start: int, i: int, stop: int) : int =
              Int.div (stop + i, 2)
          val j' = j

          fun loop (start: int, i: int, stop: int) : unit =
              if i >= j then
                ()
              else
                let fun getMid (i: int) : int = computeMid (start, i, stop)
                    val j = Int.min (j, stop)
                    fun innerLoop (i: int) : unit =
                        if i >= j then
                          ()
                        else
                          let fun body _ = iter i
                              fun seq _ = innerLoop (i + 1)
                              fun spwn _ = loop (start, i + 1, getMid (i + 1))
                              fun sync _ = ()
                          in
                            spork (body, seq, spwn, sync)
                          end
                    fun firstHalf () : unit = innerLoop i
                    fun secondHalf () : unit =
                        let val mid = getMid i in
                          loop (mid, mid, stop)
                        end
                in
                  par (firstHalf, secondHalf); ()
                end
      in
        loop (i, i, j'); ()
      end

  fun pareduce'' (i: int, j: int, z: 'a, iter: int -> 'a, merge : 'a * 'a -> 'a) : 'a =
      let (*fun exp_ceil_log n =
              (* Computes 2^ceil(log(n)),
                 i.e. rounds n up to the nearest power of 2 *)
              let fun h m =
                      if m >= n then m else h (m + m)
              in h 1 end
          fun computeMid (start: int, i: int, stop: int) : int =
              let val mid = Int.div (start + stop, 2) in
                if mid < i then
                  computeMid (mid, i, stop)
                else
                  mid
              end
          val j' = i + exp_ceil_log (j - i)*)

          fun computeMid (start: int, i: int, stop: int) : int =
              Int.div (stop + i, 2)
          val j' = j

          fun loop (start: int, i: int, stop: int, a : 'a) : 'a =
              if i >= j then
                a
              else
                let fun getMid (i: int) : int = computeMid (start, i, stop)
                    val j = Int.min (j, stop)
                    fun firstHalfLoop (i: int, a : 'a) : 'a =
                        if i >= j then
                          a
                        else
                          let fun body () : 'a = merge (a, iter i)
                              fun seq (a) : 'a = firstHalfLoop (i + 1, a)
                              fun spwn () : 'a = loop (start, i + 1, getMid (i + 1), z)
                              val sync : 'a * 'a -> 'a = merge
                          in
                            spork (body, spwn, seq, sync)
                          end
                    fun firstHalf () : 'a = firstHalfLoop (i, a)
                    fun secondHalf (a : 'a) : 'a =
                        let val mid = getMid i in
                          loop (mid, mid, stop, a)
                        end

                    fun body () : 'a = firstHalf ()
                    val seq : 'a -> 'a = secondHalf
                    fun spwn () : 'a = secondHalf z
                    val sync : 'a * 'a -> 'a = merge
                in
                  spork (body, spwn, seq, sync)
                end
      in
        loop (i, i, j', z)
      end

  fun parfor_grained grain (i, j) f =
    if j - i <= grain then
      for (i, j) f
    else
      let
        val mid = i + (j-i) div 2
      in
        par (fn _ => parfor_grained grain (i, mid) f,
             fn _ => parfor_grained grain (mid, j) f)
        ; ()
      end

  fun alloc n =
    let
      val a = ArrayExtra.Raw.alloc n
      val _ =
        if ArrayExtra.Raw.uninitIsNop a then ()
        else parfor_grained 10000 (0, n) (fn i => ArrayExtra.Raw.unsafeUninit (a, i))
    in
      ArrayExtra.Raw.unsafeToArray a
    end

  val maxForkDepthSoFar = Scheduler.maxForkDepthSoFar
  val numSpawnsSoFar = Scheduler.numSpawnsSoFar
  val numEagerSpawnsSoFar = Scheduler.numEagerSpawnsSoFar
  val numHeartbeatsSoFar = Scheduler.numHeartbeatsSoFar
  val numSkippedHeartbeatsSoFar = Scheduler.numSkippedHeartbeatsSoFar
  val numStealsSoFar = Scheduler.numStealsSoFar

  val idleTimeSoFar = Scheduler.IdleTimer.cumulative
  val workTimeSoFar = Scheduler.WorkTimer.cumulative

  fun communicate () = ()
end
