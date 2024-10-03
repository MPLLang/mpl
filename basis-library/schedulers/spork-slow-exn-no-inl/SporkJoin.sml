structure SporkJoin :>
sig
  include SPORK_JOIN
  val numSpawnsSoFar: unit -> int
  val numEagerSpawnsSoFar: unit -> int
  val numHeartbeatsSoFar: unit -> int
  val numSkippedHeartbeatsSoFar: unit -> int
  val numStealsSoFar: unit -> int
end =
struct
  type word = Word64.word
  fun w2i w = Word64.toIntX w
  fun i2w i = Word64.fromInt i

  fun sporkFair y = Scheduler.SporkJoin.sporkFair y
  fun sporkGive y = Scheduler.SporkJoin.sporkGive y
  fun sporkKeep y = Scheduler.SporkJoin.sporkKeep y
  fun sporkFair' y = Scheduler.SporkJoin.sporkFair' y
  fun sporkGive' y = Scheduler.SporkJoin.sporkGive' y
  fun sporkKeep' y = Scheduler.SporkJoin.sporkKeep' y

  (* fun specialize (f: 'a -> 'b): 'a -> 'b = *)
  (*     let fun specializedF a = f a in *)
  (*       fn a => specializedF a *)
  (*     end *)

  fun par (f: unit -> 'a, g: unit -> 'b): 'a * 'b =
      sporkFair {
        body = fn () => f (),
        spwn = fn () => g (),
        seq  = fn a => (a, g ()),
        sync = fn ab => ab
      }

  val fork = par

  (* ======================================================================= *)
  fun midpoint (i: word, j: word) =
        i + (Word64.>> (j - i, 0w1))

  fun pareduce (i: int, j: int) (z: 'a) (step: int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun iter (b: 'a) (i: word, j: word): 'a =
              if i >= j then b else
                let val i' = i + 0w1
                    fun spwn b' =
                        if i' >= j then b' else
                          let val mid = midpoint (i', j) in
                            sporkFair {
                              body = fn () => iter b' (i', mid),
                              spwn = fn () => iter z (mid, j),
                              seq  = fn b' => iter b' (mid, j),
                              sync = merge
                          }
                          end
                in
                  sporkGive' {
                    body = fn () => step (w2i i, b),
                    spwn = fn () => spwn z,
                    seq = fn b' => iter b' (i', j),
                    sync = merge,
                    unstolen = spwn
                  }
                end
      in
        iter z (i2w i, i2w j)
      end

  fun pareduceSplit (i: int, j: int) (z: 'a) (step: int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun split (b: 'a) (i: word, j: word): 'a =
              if i >= j then b else
                let val mid = midpoint (i + 0w1, j) in
                  sporkFair {
                    body = fn () =>
                              let fun loop b i =
                                      if i >= mid then b else
                                        sporkGive' {
                                          body = fn () => step (w2i i, b),
                                          spwn = fn () => split z (i + 0w1, mid),
                                          seq  = fn b' => loop b' (i + 0w1),
                                          sync = merge,
                                          unstolen = fn b' => split b' (i + 0w1, mid)
                                        }
                              in
                                loop b i
                              end,
                    spwn = fn () => split z (mid, j),
                    seq  = fn b' => split b' (mid, j),
                    sync = merge
                  }
                end
      in
        split z (i2w i, i2w j)
      end

  fun parfor (grain: int) (i: int, j: int) (f: int -> unit): unit =
      pareduce (i, j) () (fn (i, ()) => f i) (fn ((), ()) => ())

  fun alloc n =
    let
      fun for (i, j) f = if i >= j then () else (f i; for (i+1, j) f)
      val a = ArrayExtra.Raw.alloc n
      val grain = 10000
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

structure ForkJoin = SporkJoin
