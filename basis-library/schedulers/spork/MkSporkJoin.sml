functor MkSporkJoin
  (val sporkFair: (unit -> 'a) * (unit -> 'b) * ('a -> 'c) * ('a * 'b -> 'c) -> 'c
   val sporkGive: (unit -> 'a) * (unit -> 'b) * ('a -> 'c) * ('a * 'b -> 'c) -> 'c
   val sporkKeep: (unit -> 'a) * (unit -> 'b) * ('a -> 'c) * ('a * 'b -> 'c) -> 'c
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

  val sporkFair = sporkFair
  val sporkGive = sporkGive
  val sporkKeep = sporkKeep
  
  fun for (i, j) f =
    if i >= j then () else (f i; for (i + 1, j) f)

  fun par (f: unit -> 'a, g: unit -> 'b) : 'a * 'b =
    sporkFair (f, g, fn a => (a, g ()), fn (a, b) => (a, b))

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
        sporkGive (body, spwn, seq, sync)
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
              if i + 1 >= j then
                if i >= j then (* all iterations done *)
                  a
                else (* last iteration *)
                  step (i, a)
              else
                sporkGive
                  (fn () => step (i, a),
                   fn () => let val mid = (i + j) div 2
                                fun left_half () = loop (z, i + 1, mid)
                                fun right_half () = loop (z, mid, j)
                            in
                              merge (par (left_half, right_half))
                            end,
                  fn (a') => loop (a', i + 1, j),
                  merge)
      in
        loop (z, i, j)
      end

  fun pareduce' (i: int, j: int, z: 'a, iter: int -> 'a, merge: 'a * 'a -> 'a) : 'a =
      let fun loop (a: 'a, i: int, j: int) =
              if i + 1 >= j then
                if i >= j then (* all iterations done *)
                  a
                else (* last iteration *)
                  merge (a, iter i)
              else
                sporkGive
                  (fn () => merge (a, iter i),
                   fn () => let val mid = (i + j) div 2
                                fun left_half () = loop (z, i + 1, mid)
                                fun right_half () = loop (z, mid, j)
                            in
                              merge (par (left_half, right_half))
                            end,
                   fn (a') => loop (a', i + 1, j),
                   merge)
      in
        loop (z, i, j)
      end

  fun par (f: unit -> 'a, g: unit -> 'b) : 'a * 'b =
      sporkFair (f, g, fn (a) => (a, g ()), fn (ab) => ab)

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
                            sporkGive (body, seq, spwn, sync)
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
      let fun loop (i: int, stop: int, a : 'a) : 'a =
              if i + 1 >= stop then
                if i >= stop then
                  a
                else
                  merge (a, iter i)
              else
                let val mid = (i + stop) div 2
                    fun firstHalfLoop (i: int, a : 'a) : 'a =
                        if i + 1 >= mid then
                          if i >= mid then
                            a
                          else
                            merge (a, iter i)
                        else
                          let fun body2 () : 'a = merge (a, iter i)
                              fun seq2 (a) : 'a = firstHalfLoop (i + 1, a)
                              fun spwn2 () : 'a = loop (i + 1, mid, z)
                              val sync2 : 'a * 'a -> 'a = merge
                          in
                            sporkGive (body2, spwn2, seq2, sync2)
                          end
                    fun body () : 'a = firstHalfLoop (i, a)
                    fun seq (a) : 'a = loop (mid, stop, a)
                    fun spwn () : 'a = loop (mid, stop, z)
                    val sync : 'a * 'a -> 'a = merge
                in
                  sporkFair (body, spwn, seq, sync)
                end
      in
        loop (i, j, z)
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