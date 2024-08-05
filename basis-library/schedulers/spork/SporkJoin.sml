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
  val w2i = Word64.toIntX
  val i2w = Word64.fromInt

  val sporkFair = Scheduler.SporkJoin.sporkFair
  val sporkGive = Scheduler.SporkJoin.sporkGive
  val sporkKeep = Scheduler.SporkJoin.sporkKeep
  val sporkSamFair = Scheduler.SporkJoin.sporkSamFair
  val sporkSamGive = Scheduler.SporkJoin.sporkSamGive
  val sporkSamKeep = Scheduler.SporkJoin.sporkSamKeep
  
  fun for (i: word, j: word) (f: word -> unit): unit =
      if i >= j then
        ()
      else
        (f i;
         for (i + 0w1, j) f)

  fun reduce (i: word, j: word) (a: 'a) (step: word * 'a -> 'a): 'a =
      if i >= j then
        a
      else
        reduce (i + 0w1, j) (step (i, a)) step

  fun par (f: unit -> 'a, g: unit -> 'b): 'a * 'b =
      sporkFair (f, g, fn a => (a, g ()), fn ab => ab)

  fun midpoint (i: word, j: word) =
      i + (Word64.>> (j - i, 0w1))

  val fork = par

  (* ======================================================================= *)

  fun wpareduce (grain: word) (i: word, j: word) (z: 'a) (step: word * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun seqred (b: 'a) (i: word, j: word) =
              reduce (i, j) b step
          fun splitCheckBoundsTokens (b: 'a) (i: word, j: word): 'a =
              if grain >= j - i then
                seqred b (i, j)
              else
                splitCheckTokens b (i, j)
          and splitCheckBounds (b: 'a) (i: word, j: word): 'a =
              if grain >= j - i then
                seqred b (i, j)
              else
                split b (i, j)
          and splitCheckTokens (b: 'a) (i: word, j: word): 'a =
              if Scheduler.SporkJoin.noTokens () then
                iter b (i, j)
              else
                split b (i, j)
          and split (b: 'a) (i: word, j: word): 'a =
              let val mid = midpoint (i, j) in
                  merge (par (fn () => splitCheckBoundsTokens b (i, mid),
                              fn () => splitCheckBoundsTokens z (mid, j)))
                end
          and iterCheckBounds (b: 'a) (i: word, j: word): 'a =
              if grain >= j - i then
                seqred b (i, j)
              else
                iter b (i, j)
          and iter (b: 'a) (i: word, j: word): 'a =
              let val i' = i + grain
                  fun unstolen b' = splitCheckBounds b' (i', j)
                  fun body () = seqred b (i, i')
                  fun spwn () = unstolen z
                  fun seq b' = iterCheckBounds b' (i', j)
                  val sync = merge
              in
                sporkSamGive (body, spwn, seq, sync, unstolen)
              end
      in
        splitCheckBoundsTokens z (i, j)
      end

  fun wpareduceInitStepMerge (grain: word) (i: word, j: word) (init: word -> 'a) (step: word * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun seqred (b: 'a) (i: word, j: word): 'a =
              reduce (i, j) b step

          fun seqredNZ (i: word, j: word): 'a =
              if j <= i then
                Scheduler.die (fn () => "seqredNZ but range has zero size")
              else
                seqred (init i) (i + 0w1, j)

          fun splitCheckBoundsTokensNZ (i: word, j: word): 'a =
              if grain >= j - i then
                seqredNZ (i, j)
              else
                splitCheckTokensNZ (i, j)
          and splitCheckBoundsNZ (i: word, j: word): 'a =
              if grain >= j - i then
                seqredNZ (i, j)
              else
                splitNZ (i, j)
          and splitCheckTokensNZ (i: word, j: word): 'a =
              if Scheduler.SporkJoin.noTokens () then
                iterNZ (i, j)
              else
                splitNZ (i, j)
          and splitNZ (i: word, j: word): 'a =
              let val mid = midpoint (i, j) in
                  merge (par (fn () => splitCheckBoundsTokensNZ (i, mid),
                              fn () => splitCheckBoundsTokensNZ (mid, j)))
              end
          and iterNZ (i: word, j: word): 'a =
              let val i' = i + grain
                  fun unstolen b' = splitCheckBounds b' (i', j)
                  fun body () = seqredNZ (i, i')
                  fun spwn () = splitCheckBoundsNZ (i', j)
                  fun seq b' = iterCheckBounds b' (i', j)
                  val sync = merge
              in
                sporkSamGive (body, spwn, seq, sync, unstolen)
              end

          and splitCheckBoundsTokens (b: 'a) (i: word, j: word): 'a =
              if grain >= j - i then
                seqred b (i, j)
              else
                splitCheckTokens b (i, j)
          and splitCheckBounds (b: 'a) (i: word, j: word): 'a =
              if grain >= j - i then
                seqred b (i, j)
              else
                split b (i, j)
          and splitCheckTokens (b: 'a) (i: word, j: word): 'a =
              if Scheduler.SporkJoin.noTokens () then
                iter b (i, j)
              else
                split b (i, j)
          and split (b: 'a) (i: word, j: word): 'a =
              let val mid = midpoint (i, j) in
                  merge (par (fn () => splitCheckBoundsTokens b (i, mid),
                              fn () => splitCheckBoundsTokensNZ (mid, j)))
              end
          and iterCheckBounds (b: 'a) (i: word, j: word): 'a =
              if grain >= j - i then
                seqred b (i, j)
              else
                iter b (i, j)
          and iter (b: 'a) (i: word, j: word): 'a =
              let val i' = i + grain
                  fun unstolen b' = splitCheckBounds b' (i', j)
                  fun body () = seqred b (i, i')
                  fun spwn () = splitCheckBoundsNZ (i', j)
                  fun seq b' = iterCheckBounds b' (i', j)
                  val sync = merge
              in
                sporkSamGive (body, spwn, seq, sync, unstolen)
              end
      in
        splitCheckBoundsTokensNZ (i, j)
      end

  fun wpareduce' (grain: word) (i: word, j: word) (z: 'a) (iter: word -> 'a) (merge: 'a * 'a -> 'a): 'a =
      wpareduce grain (i, j) z (fn (i, a) => merge (a, iter i)) merge

  fun wpareduce_sam (grain: word) (i: word, j: word) (z: 'a) (iter: word -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun seqred (a: 'a, i: word, j: word): 'a =
              reduce (i, j) a (fn (i, a) => merge (a, iter i))
          fun loop (a: 'a) (i: word, j: word) =
              let val i' = i + grain in
                if i' >= j then
                  seqred (a, i, j)
                else
                  let fun unstolen b =
                          let val mid = midpoint (i', j) in
                            merge (par (fn () => loop b (i', mid),
                                        fn () => loop z (mid, j)))
                          end
                      fun body (): 'a = seqred (a, i, i')
                      fun spwn (): 'a = unstolen z
                      fun seq (a'): 'a = loop a' (i', j)
                      val sync = merge
                  in
                    sporkSamGive (body, spwn, seq, sync, unstolen)
                  end
              end
      in
        loop z (i, j)
      end

  fun wparfor_sam (grain: word) (i: word, j: word) (iter: word -> unit): unit =
      let fun seqfor (i: word, j: word): unit =
              for (i, j) iter
          fun loop (i: word, j: word): unit =
              let val i' = i + grain in
                if i' >= j then
                  seqfor (i, j)
                else
                  let fun body (): unit = seqfor (i, i')
                      fun spwn (): unit =
                          let val mid = midpoint (i', j) in
                            par (fn () => loop (i', mid),
                                 fn () => loop (mid, j));
                            ()
                          end
                      fun seq (): unit = loop (i', j)
                      fun sync ((), ()): unit = ()
                      fun unstolen () = spwn ()
                  in
                    sporkSamGive (body, spwn, seq, sync, unstolen)
                  end
              end
      in
        loop (i, j)
      end


  fun wpareduce_simple (grain: word) (i: word, j: word) (z: 'a) (iter: word -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun seqred (a: 'a, i: word, j: word): 'a =
              reduce (i, j) a (fn (i, a) => merge (a, iter i))
          fun loop (a: 'a) (i: word, j: word) =
              let val i' = i + grain in
                if i' >= j then
                  seqred (a, i, j)
                else
                  let fun body (): 'a = seqred (a, i, i')
                      fun spwn (): 'a =
                          let val mid = midpoint (i', j) in
                            merge (par (fn () => loop z (i', mid),
                                        fn () => loop z (mid, j)))
                          end
                      fun seq (a'): 'a = loop a' (i', j)
                      val sync = merge
                  in
                    sporkGive (body, spwn, seq, sync)
                  end
              end
      in
        loop z (i, j)
      end

  fun wparfor_simple (grain: word) (i: word, j: word) (iter: word -> unit): unit =
      let fun seqfor (i: word, j: word): unit =
              for (i, j) iter
          fun loop (i: word, j: word): unit =
              let val i' = i + grain in
                if i' >= j then
                  seqfor (i, j)
                else
                  let fun body (): unit = seqfor (i, i')
                      fun spwn (): unit =
                          let val mid = midpoint (i', j) in
                            par (fn () => loop (i', mid),
                                 fn () => loop (mid, j));
                            ()
                          end
                      fun seq (): unit = loop (i', j)
                      fun sync ((), ()): unit = ()
                  in
                    sporkGive (body, spwn, seq, sync)
                  end
              end
      in
        loop (i, j)
      end
(*
  fun umut_parfor (i, j) f =
      let val range = Word.fromWord (j - i)
          val logrange = Word.log2 range
          val loglogrange = Word.log2 logrange in
        TODO
      end
*)
(*
          fun loop (i, j) =
              let val body = f i
                  val spwn = umut_parfor (i + 1, j) f
                  val seq = loop (i + 1, j)
                  val sync = ()
              in
                spork body spwn seq sync
              end
      in
        hard_fork (fn () => loop (i, (i + j) / 2))
                  (fn () => umut_parfor (((i + j) / 2), j) f)
      end*)

  fun wpareduce'' (grain: word) (i: word, j: word) (z: 'a) (iter: word -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun seqred (a: 'a, i: word, j: word): 'a =
              reduce (i, j) a (fn (i, a) => merge (a, iter i))
          fun loop (i: word, j: word): 'a =
              if i + grain >= j then
                seqred (z, i, j)
              else
                let val mid = midpoint (i, j) in
                  merge (par (fn () => innerLoop z (i, mid),
                              fn () => innerLoop z (mid, j)))
                end
          and innerLoop (a: 'a) (i: word, j: word): 'a =
              let val i' = i + grain in
                if i' >= j then
                  seqred (a, i, j)
                else
                  let fun body () = seqred (a, i, i')
                      fun spwn () = loop (i', j)
                      fun seq (a) = innerLoop a (i', j)
                      val sync = merge
                  in
                    sporkGive (body, spwn, seq, sync)
                  end
              end
      in
        if i >= j then
          z
        else
          loop (i, j)
      end

  fun pareduce (grain: int) (i: int, j: int) (b: 'a) (step: int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      wpareduce (i2w grain) (i2w i, i2w j) b (fn (w, a) => step (w2i w, a)) merge
  fun pareduce' (grain: int) (i: int, j: int) (b: 'a) (iter: int -> 'a) (merge: 'a * 'a -> 'a): 'a =
      wpareduce' (i2w grain) (i2w i, i2w j) b (iter o w2i) merge
  fun pareduce'' (grain: int) (i: int, j: int) (b: 'a) (iter: int -> 'a) (merge: 'a * 'a -> 'a): 'a =
      wpareduce'' (i2w grain) (i2w i, i2w j) b (iter o w2i) merge
  fun pareduce_sam (grain: int) (i: int, j: int) (b: 'a) (iter: int -> 'a) (merge: 'a * 'a -> 'a): 'a =
      wpareduce_sam (i2w grain) (i2w i, i2w j) b (iter o w2i) merge
  fun pareduce_simple (grain: int) (i: int, j: int) (b: 'a) (iter: int -> 'a) (merge: 'a * 'a -> 'a): 'a =
      wpareduce_simple (i2w grain) (i2w i, i2w j) b (iter o w2i) merge

  fun parfor (grain: int) (i: int, j: int) (iter: int -> unit): unit =
      pareduce grain (i, j) () (fn (i, ()) => iter i) (fn ((), ()) => ())
  fun parfor'' (grain: int) (i: int, j: int) (iter: int -> unit): unit =
      pareduce'' grain (i, j) () iter (fn ((), ()) => ())
  fun parfor_sam (grain: int) (i: int, j: int) (iter: int -> unit): unit =
      pareduce_sam grain (i, j) () iter (fn ((), ()) => ())
  fun parfor_simple (grain: int) (i: int, j: int) (iter: int -> unit): unit =
      pareduce_simple grain (i, j) () iter (fn ((), ()) => ())

  fun pareduceInitStepMerge (grain: int) (i: int, j: int) (init: int -> 'a) (step: int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      wpareduceInitStepMerge (i2w grain) (i2w i, i2w j) (init o w2i) (fn (w, a) => step (w2i w, a)) merge

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
