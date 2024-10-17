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
  fun __inline_always__ w2i w = __inline_always__ Word64.toIntX w
  fun __inline_always__ i2w i = __inline_always__ Word64.fromInt i

  fun __inline_always__ sporkFair y = __inline_always__ Scheduler.SporkJoin.sporkFair y
  fun __inline_always__ sporkGive y = __inline_always__ Scheduler.SporkJoin.sporkGive y
  fun __inline_always__ sporkKeep y = __inline_always__ Scheduler.SporkJoin.sporkKeep y
  fun __inline_always__ sporkFair' y = __inline_always__ Scheduler.SporkJoin.sporkFair' y
  fun __inline_always__ sporkGive' y = __inline_always__ Scheduler.SporkJoin.sporkGive' y
  fun __inline_always__ sporkKeep' y = __inline_always__ Scheduler.SporkJoin.sporkKeep' y

  (* fun __inline_always__ specialize (f: 'a -> 'b): 'a -> 'b = *)
  (*     let fun specializedF a = f a in *)
  (*       fn __inline_always__ a => __inline_never__ specializedF a *)
  (*     end *)

  fun par (f: unit -> 'a, g: unit -> 'b): 'a * 'b =
      sporkFair {
        body = f,
        spwn = g,
        seq  = fn a => (a, g ()),
        sync = fn ab => ab
      }

  val fork = par

  (* ======================================================================= *)
  fun __inline_always__ midpoint (i: word, j: word) =
        i + (Word64.>> (j - i, 0w1))

  (* fun __inline_always__ pareduce (i: int, j: int) (z: 'a) (step: int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a = *)
  (*     let fun iter (b: 'a) (i: word, j: word): 'a = *)
  (*             if i >= j then b else *)
  (*               let val i' = i + 0w1 *)
  (*                   fun __inline_never__ spwn b' = *)
  (*                       if i' >= j then b' else *)
  (*                         let val mid = midpoint (i', j) in *)
  (*                           sporkFair { *)
  (*                             body = fn () => iter b' (i', mid), *)
  (*                             spwn = fn () => iter z (mid, j), *)
  (*                             seq  = fn b' => iter b' (mid, j), *)
  (*                             sync = merge *)
  (*                         } *)
  (*                         end *)
  (*               in *)
  (*                 sporkGive' { *)
  (*                   body = fn () => __inline_always__ step (w2i i, b), *)
  (*                   spwn = fn () => spwn z, *)
  (*                   seq = fn b' => iter b' (i', j), *)
  (*                   sync = merge, *)
  (*                   unstolen = spwn *)
  (*                 } *)
  (*               end *)
  (*     in *)
  (*       __inline_always__ iter z (i2w i, i2w j) *)
  (*     end *)


  fun __inline_always__ pareduceBreak (i: int, j: int) (z: 'a) (step: int * 'a -> 'a * bool) (merge: 'a * 'a -> 'a): 'a =
      let fun merge' ((b1, cont1), (b2, cont2)) =
              if cont1 then (merge (b1, b2), cont2) else (b1, false)

          fun continue (f : 'a -> 'a * bool) : 'a * bool -> 'a * bool =
              fn (b, cont) => if cont then f b else (b, cont)

          fun iter (b: 'a) (i: word, j: word): 'a * bool =
              if i >= j then (b, true) else
                let val i' = i + 0w1
                    fun __inline_never__ spwn b' =
                        if i' >= j then (b', true) else
                          let val mid = midpoint (i', j) in
                            sporkFair {
                              body = fn () => iter b' (i', mid),
                              spwn = fn () => iter z (mid, j),
                              seq = continue (fn b' => iter b' (mid, j)),
                              sync = merge'
                          }
                          end
                in
                  sporkGive' {
                    body = fn () => __inline_always__ step (w2i i, b),
                    spwn = fn () => spwn z,
                    seq = continue (fn b' => iter b' (i', j)),
                    sync = merge',
                    unstolen = continue spwn
                  }
                end
          val (result, cont) = __inline_always__ iter z (i2w i, i2w j)
      in
        result
      end


  fun __inline_always__ pareduceBreakExn (i: int, j: int) (z: 'a) (step: ('a -> exn) * int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let exception Break of 'a in
        pareduceBreak (i, j) z (fn (i, a) => (__inline_always__ step (Break, i, a), true) handle (Break b) => (b, false)) merge
      end

  (* fun __inline_always__ pareduceBreakExn (i: int, j: int) (z: 'a) (step: ('a -> exn) * int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a = *)
  (*     let exception Break of 'a *)
  (*         fun merge' ((b1, cont1), (b2, cont2)) = *)
  (*             if cont1 then (merge (b1, b2), cont2) else (b1, cont1) *)

  (*         fun continue (f : 'a -> 'a * bool) : 'a * bool -> 'a * bool = *)
  (*             fn (b, cont) => if cont then f b else (b, cont) *)

  (*         fun iter (b: 'a) (i: word, j: word): 'a * bool = *)
  (*             if i >= j then (b, true) else *)
  (*               let val i' = i + 0w1 *)
  (*                   fun __inline_never__ spwn b' = *)
  (*                       if i' >= j then (b', true) else *)
  (*                         let val mid = midpoint (i', j) in *)
  (*                           sporkFair { *)
  (*                             body = fn () => iter b' (i', mid), *)
  (*                             spwn = fn () => iter z (mid, j), *)
  (*                             seq = continue (fn b' => iter b' (mid, j)), *)
  (*                             sync = merge' *)
  (*                         } *)
  (*                         end *)
  (*               in *)
  (*                 sporkGive' { *)
  (*                   body = fn () => (__inline_always__ step (Break, w2i i, b), true) handle (Break b) => (b, false), *)
  (*                   spwn = fn () => spwn z, *)
  (*                   seq = continue (fn b' => iter b' (i', j)), *)
  (*                   sync = merge', *)
  (*                   unstolen = continue spwn *)
  (*                 } *)
  (*               end *)
  (*         val (result, cont) = __inline_always__ iter z (i2w i, i2w j) *)
  (*     in *)
  (*       result *)
  (*     end *)

  fun __inline_always__ pareduce (i: int, j: int) (z: 'a) (step: int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      pareduceBreakExn (i, j) z (fn (break, i, a) => step (i, a)) merge

  fun __inline_always__ pareduceSplit (i: int, j: int) (z: 'a) (step: int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun split (b: 'a) (i: word, j: word): 'a =
              if i >= j then b else
                let val mid = midpoint (i + 0w1, j) in
                  sporkFair {
                    body = fn __inline_always__ () =>
                              let fun loop b i =
                                      if i >= mid then b else
                                        sporkGive' {
                                          body = fn () => __inline_always__ step (w2i i, b),
                                          spwn = fn () => split z (i + 0w1, mid),
                                          seq  = fn b' => loop b' (i + 0w1),
                                          sync = merge,
                                          unstolen = fn b' => split b' (i + 0w1, mid)
                                        }
                              in
                                __inline_always__ loop b i
                              end,
                    spwn = fn () => split z (mid, j),
                    seq  = fn b' => split b' (mid, j),
                    sync = merge
                  }
                end
      in
        __inline_always__ split z (i2w i, i2w j)
      end

  fun __inline_always__ parfor (grain: int) (i: int, j: int) (f: int -> unit): unit =
      pareduce (i, j) () (fn (i, ()) => __inline_always__ f i) (fn ((), ()) => ())

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
