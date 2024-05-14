functor MkSporkJoin
  (val spork: (unit -> 'a) * (unit -> 'b) * ('a -> 'c) * ('a * 'b -> 'c) -> 'c
   val fork: (unit -> 'a) * (unit -> 'b) -> 'a * 'b) :>
sig
  include SPORK_JOIN
  val numSpawnsSoFar: unit -> int
  val numEagerSpawnsSoFar: unit -> int
  val numHeartbeatsSoFar: unit -> int
  val numSkippedHeartbeatsSoFar: unit -> int
  val numStealsSoFar: unit -> int
end =
struct
  val spork = spork
  val fork = fork
  val eager_par = fork

  fun for (i, j) f = if i >= j then () else (f i; for (i+1, j) f)

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
                                val (al, ar) = eager_par (left_half, right_half)
                            in
                              merge (al, ar)
                            end,
                  fn (a') => loop (a', i + 1, j),
                  merge)
      in
        loop (z, i, j)
      end

  fun pareduce' (i, j, z, iter, merge) =
      let fun loop (a, i, j) =
              if i >= j then
                a
              else
                spork
                  (fn () => merge (a, iter i),
                   fn () => let val mid = (i + j) div 2
                                fun left_half () = loop (z, i + 1, mid)
                                fun right_half () = loop (z, mid, j)
                                val (al, ar) = eager_par (left_half, right_half)
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

  fun parfor grain (i, j) f =
    if j - i <= grain then
      for (i, j) f
    else
      let
        val mid = i + (j-i) div 2
      in
        par (fn _ => parfor grain (i, mid) f,
             fn _ => parfor grain (mid, j) f)
        ; ()
      end

  fun alloc n =
    let
      val a = ArrayExtra.Raw.alloc n
      val _ =
        if ArrayExtra.Raw.uninitIsNop a then ()
        else parfor 10000 (0, n) (fn i => ArrayExtra.Raw.unsafeUninit (a, i))
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
