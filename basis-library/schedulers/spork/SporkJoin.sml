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

  fun par (f: unit -> 'a, g: unit -> 'b): 'a * 'b =
      sporkFair (f, g, fn a => (a, g ()), fn ab => ab)

  val fork = par

  (* ======================================================================= *)

  local
    fun reduce (i: word, j: word) (a: 'a) (step: word * 'a -> 'a): 'a =
        if i + 0w1 >= j then
          if i >= j then
            a
          else
            step (i, a)
        else
          reduce (i + 0w1, j) (step (i, a)) step

    fun midpoint (i: word, j: word) =
        i + (Word64.>> (j - i, 0w1))
  in

  (*fun wpareduce_grained (grain: word) (i: word, j: word) (z: 'a) (step: word * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun red_no_grain (i, j) b =
              if i = j then
                b
              else
                step (i, b)
          fun red_grain (i, j) b =
              reduce (i, j) b step
          val seqred =
              if grain = 0w1 then
                red_no_grain
              else
                red_grain

          fun steps_no_grain (i, i') b =
              step (i, b)

          val steps =
              if grain = 0w1 then
                steps_no_grain
              else
                red_grain
          fun guard (f: 'a -> word * word -> 'a) (b: 'a) (i: word, j: word): 'a =
              if grain >= j - i then
                seqred (i, j) b
              else
                f b (i, j)
          fun splitCheckBoundsTokens (b: 'a) (i: word, j: word): 'a =
              guard splitCheckTokens b (i, j)
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
          and iter (b: 'a) (i: word, j: word): 'a =
              let val i' = i + grain
                  fun unstolen b' = guard split b' (i', j)
                  fun body () = steps (i, i') b
                  fun spwn () = unstolen z
                  fun seq b' = guard iter b' (i', j)
                  val sync = merge
              in
                sporkSamGive (body, spwn, seq, sync, unstolen)
              end
      in
        splitCheckBoundsTokens z (i, j)
      end*)

  fun wpareduce (i: word, j: word) (z: 'a) (step: word * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun guard (f: 'a -> word * word -> 'a) (b: 'a) (i: word, j: word): 'a =
              if i + 0w1 >= j then
                if i >= j then
                  b
                else
                  step (i, b)
              else
                f b (i, j)
          fun splitCheckBoundsTokens (b: 'a) (i: word, j: word): 'a =
              guard splitCheckTokens b (i, j)
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
          and iter (b: 'a) (i: word, j: word): 'a =
              let val i' = i + 0w1
                  fun unstolen b' = guard split b' (i', j)
                  fun body () = step (i, b)
                  fun spwn () = unstolen z
                  fun seq b' = guard iter b' (i', j)
                  val sync = merge
              in
                sporkSamGive (body, spwn, seq, sync, unstolen)
              end
      in
        splitCheckBoundsTokens z (i, j)
      end

  (* IDEA: maybe have a wrapper loop that always gets inlined, then once promoted go to funcall version *)

  fun wpareduceInlineable (i: word, j: word) (z: 'a) (step: word * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun loop (b: 'a) (i: word, j: word) =
              let val i' = i + 0w1 in
                if i' >= j then
                  if i >= j then b else step (i, b)
                else
                  let fun body () = step (i, b)
                      fun seq b' = loop b' (i', j)
                      fun spwn () =
                          let val mid = midpoint (i', j) in
                            merge (par (fn () => wpareduce (i', mid) z step merge,
                                        fn () => wpareduce (mid, j) z step merge))
                          end
                      val sync = merge
                      val unstolen = seq
                      (* fun unstolen b' = merge (b', spwn () *)
                  in
                    sporkSamGive (body, spwn, seq, sync, unstolen)
                  end
              end
      in
        loop z (i, j)
      end

  fun wpareduceInlineable' (i: word, j: word) (z: 'a) (step: word * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun guard (f: 'a -> word * word -> 'a) (b: 'a) (i: word, j: word): 'a =
              if i + 0w1 >= j then
                if i >= j then
                  b
                else
                  step (i, b)
              else
                f b (i, j)
          fun splitCheckBoundsTokens (b: 'a) (i: word, j: word): 'a =
              guard splitCheckTokens b (i, j)
          and splitCheckTokens (b: 'a) (i: word, j: word): 'a =
              if Scheduler.SporkJoin.noTokens () then
                iter b (i, j)
              else
                split b (i, j)
          and split (b: 'a) (i: word, j: word): 'a =
              let val mid = midpoint (i, j)
                  fun body () = splitCheckBoundsTokens b (i, mid)
                  fun spwn () = splitCheckBoundsTokens z (mid, j)
                  fun seq b' = splitCheckBoundsTokens b' (mid, j)
                  val sync = merge
                  val unstolen  = seq
              in
                sporkSamFair (body, spwn, seq, sync, unstolen)
              end
          and iter (b: 'a) (i: word, j: word): 'a =
              let val i' = i + 0w1
                  fun body () = step (i, b)
                  fun spwn () = guard split z (i', j)
                  fun seq b' = guard iter b' (i', j)
                  val sync = merge
                  fun unstolen b' = guard split b' (i', j)
              in
                sporkSamGive (body, spwn, seq, sync, unstolen)
              end
      in
        splitCheckBoundsTokens z (i, j)
      end


    (* This ended up slowing down performance SIGNIFICANTLY -- perhaps because it crosses the inlining threshold? *)
    (* fun wpareduce (grain: word) (i: word, j: word) (z: 'a) (step: word * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a = *)
    (*     if grain = 0w1 then *)
    (*       wpareduce_no_grain (i, j) z step merge *)
    (*     else *)
    (*       wpareduce_grained grain (i, j) z step merge *)

  (* fun wpareduce (grain: word) (i: word, j: word) (z: 'a) (step: word * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a = *)
  (*     let val _ = if grain = 0w1 then () else Scheduler.die (fn () => "Expected grain size = 1") *)
  (*         fun guard (f: 'a -> word * word -> 'a) (b: 'a) (i: word, j: word): 'a = *)
  (*             if 0w1 >= j - i then *)
  (*               if 0w1 = j - i then *)
  (*                 step (i, b) *)
  (*               else *)
  (*                 b *)
  (*             else *)
  (*               f b (i, j) *)
  (*         fun splitCheckBoundsTokens (b: 'a) (i: word, j: word): 'a = *)
  (*             guard splitCheckTokens b (i, j) *)
  (*         and splitCheckTokens (b: 'a) (i: word, j: word): 'a = *)
  (*             if Scheduler.SporkJoin.noTokens () then *)
  (*               iter b (i, j) *)
  (*             else *)
  (*               split b (i, j) *)
  (*         and split (b: 'a) (i: word, j: word): 'a = *)
  (*             let val mid = midpoint (i, j) in *)
  (*                 merge (par (fn () => splitCheckBoundsTokens b (i, mid), *)
  (*                             fn () => splitCheckBoundsTokens z (mid, j))) *)
  (*               end *)
  (*         and iter (b: 'a) (i: word, j: word): 'a = *)
  (*             let val i' = i + grain *)
  (*                 fun unstolen b' = guard split b' (i', j) *)
  (*                 fun body () = step (i, b) *)
  (*                 fun spwn () = unstolen z *)
  (*                 fun seq b' = guard iter b' (i', j) *)
  (*                 val sync = merge *)
  (*             in *)
  (*               sporkSamGive (body, spwn, seq, sync, unstolen) *)
  (*             end *)
  (*     in *)
  (*       splitCheckBoundsTokens z (i, j) *)
  (*     end *)

  fun wpareduce_sam (i: word, j: word) (z: 'a) (step: word * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun loop (a: 'a) (i: word, j: word) =
              let val i' = i + 0w1 in
                if i' >= j then
                  if i >= j then
                    a
                  else
                    step (i, a)
                else
                  let fun unstolen b =
                          let val mid = midpoint (i', j) in
                            merge (par (fn () => loop b (i', mid),
                                        fn () => loop z (mid, j)))
                          end
                      fun body (): 'a = step (i, a)
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


  (* fun wpareduce_sam (grain: word) (i: word, j: word) (z: 'a) (step: word * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a = *)
  (*     let fun seqred (a: 'a, i: word, j: word): 'a = *)
  (*             reduce (i, j) a step *)
  (*         fun loop (a: 'a) (i: word, j: word) = *)
  (*             let val i' = i + grain in *)
  (*               if i' >= j then *)
  (*                 seqred (a, i, j) *)
  (*               else *)
  (*                 let fun unstolen b = *)
  (*                         let val mid = midpoint (i', j) in *)
  (*                           merge (par (fn () => loop b (i', mid), *)
  (*                                       fn () => loop z (mid, j))) *)
  (*                         end *)
  (*                     fun body (): 'a = seqred (a, i, i') *)
  (*                     fun spwn (): 'a = unstolen z *)
  (*                     fun seq (a'): 'a = loop a' (i', j) *)
  (*                     val sync = merge *)
  (*                 in *)
  (*                   sporkSamGive (body, spwn, seq, sync, unstolen) *)
  (*                 end *)
  (*             end *)
  (*     in *)
  (*       loop z (i, j) *)
  (*     end *)

  (* fun wpareduce_simple (grain: word) (i: word, j: word) (z: 'a) (step: word * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a = *)
  (*     let fun seqred (a: 'a, i: word, j: word): 'a = *)
  (*             reduce (i, j) a step *)
  (*         fun loop (a: 'a) (i: word, j: word) = *)
  (*             let val i' = i + grain in *)
  (*               if i' >= j then *)
  (*                 seqred (a, i, j) *)
  (*               else *)
  (*                 let fun body (): 'a = seqred (a, i, i') *)
  (*                     fun spwn (): 'a = *)
  (*                         let val mid = midpoint (i', j) in *)
  (*                           merge (par (fn () => loop z (i', mid), *)
  (*                                       fn () => loop z (mid, j))) *)
  (*                         end *)
  (*                     fun seq (a'): 'a = loop a' (i', j) *)
  (*                     val sync = merge *)
  (*                 in *)
  (*                   sporkGive (body, spwn, seq, sync) *)
  (*                 end *)
  (*             end *)
  (*     in *)
  (*       loop z (i, j) *)
  (*     end *)

  fun wpareduce_simple (i: word, j: word) (z: 'a) (step: word * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun loop (a: 'a) (i: word, j: word) =
              let val i' = i + 0w1 in
                if i' >= j then
                  if i >= j then
                    a
                  else
                    step (i, a)
                else
                  let fun body (): 'a = step (i, a)
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

  fun wpareduce'' (i: word, j: word) (z: 'a) (step: word * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun loop (i: word, j: word): 'a =
              if i + 0w1 >= j then
                if i >= j then
                  z
                else
                  step (i, z)
              else
                let val mid = midpoint (i, j) in
                  merge (par (fn () => innerLoop z (i, mid),
                              fn () => innerLoop z (mid, j)))
                end
          and innerLoop (a: 'a) (i: word, j: word): 'a =
              let val i' = i + 0w1 in
                if i' >= j then
                  if i >= j then
                    a
                  else
                    step (i, a)
                else
                  let fun body () = step (i, a)
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
  end

  (* fun wpareduce'' (grain: word) (i: word, j: word) (z: 'a) (step: word * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a = *)
  (*     let fun seqred (a: 'a, i: word, j: word): 'a = *)
  (*             reduce (i, j) a step *)
  (*         fun loop (i: word, j: word): 'a = *)
  (*             if i + grain >= j then *)
  (*               seqred (z, i, j) *)
  (*             else *)
  (*               let val mid = midpoint (i, j) in *)
  (*                 merge (par (fn () => innerLoop z (i, mid), *)
  (*                             fn () => innerLoop z (mid, j))) *)
  (*               end *)
  (*         and innerLoop (a: 'a) (i: word, j: word): 'a = *)
  (*             let val i' = i + grain in *)
  (*               if i' >= j then *)
  (*                 seqred (a, i, j) *)
  (*               else *)
  (*                 let fun body () = seqred (a, i, i') *)
  (*                     fun spwn () = loop (i', j) *)
  (*                     fun seq (a) = innerLoop a (i', j) *)
  (*                     val sync = merge *)
  (*                 in *)
  (*                   sporkGive (body, spwn, seq, sync) *)
  (*                 end *)
  (*             end *)
  (*     in *)
  (*       if i >= j then *)
  (*         z *)
  (*       else *)
  (*         loop (i, j) *)
  (*     end *)
  (* end *)

  local
    fun wrap (reduce: word * word -> 'a -> (word * 'a -> 'a) -> ('a * 'a -> 'a) -> 'a)
             (i: int, j: int) (b: 'a) (step: int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
        reduce (i2w i, i2w j) b (fn (w, a) => step (w2i w, a)) merge
    fun pfor (reduce: word * word -> unit -> (word * unit -> unit) -> (unit * unit -> unit) -> unit)
             (i: int, j: int) (step: int -> unit): unit =
        reduce (i2w i, i2w j) () (fn (w, ()) => step (w2i w)) (fn ((), ()) => ())
  in
  fun pareduce (i: int, j: int) (b: 'a) (step: int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      wrap wpareduce (i, j) b step merge
  fun pareduce'' (i: int, j: int) (b: 'a) (step: int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      wrap wpareduce'' (i, j) b step merge
  fun pareduce_sam (i: int, j: int) (b: 'a) (step: int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      wrap wpareduce_sam (i, j) b step merge
  fun pareduce_simple (i: int, j: int) (b: 'a) (step: int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      wrap wpareduce_simple (i, j) b step merge
  fun pareduceInlineable (i: int, j: int) (b: 'a) (step: int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      wrap wpareduceInlineable (i, j) b step merge
  end

  (*fun parfor (grain: int) (i: int, j: int) (f: int -> unit): unit =
      let fun for (i, j) =
              if i >= j then
                ()
              else
                (f i; for (i + 1, j))
          val k = 1 + ((j - (i + 1)) div grain)
          fun f i = for (i * grain, Int.min (i * grain + grain, j))
      in
        pareduce (0, k) () (fn (i, ()) => f i) (fn ((), ()) => ())
      end*)
  fun parfor (grain: int) (i: int, j: int) (f: int -> unit): unit =
      pareduce (i, j) () (fn (i, ()) => f i) (fn ((), ()) => ())

  (*fun wpareduceInitStepMerge (grain: word) (i: word, j: word) (init: word -> 'a) (step: word * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
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
      end*)


  (* fun pareduceInitStepMerge (grain: int) (i: int, j: int) (init: int -> 'a) (step: int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a = *)
  (*     wpareduceInitStepMerge (i2w grain) (i2w i, i2w j) (init o w2i) (fn (w, a) => step (w2i w, a)) merge *)

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
