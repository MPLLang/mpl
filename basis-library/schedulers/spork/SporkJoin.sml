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
  fun wparfor (grain: word) (i: word, j: word) (step: word -> unit): unit =
      let fun seqfor (i: word, j: word): unit =
              for (i, j) step
          fun splitCheckBoundsTokens (i: word, j: word): unit =
              if grain >= j - i then
                seqfor (i, j)
              else
                splitCheckTokens (i, j)
          and splitCheckBounds (i: word, j: word): unit =
              if grain >= j - i then
                seqfor (i, j)
              else
                split (i, j)
          and splitCheckTokens (i: word, j: word): unit =
              if Scheduler.SporkJoin.noTokens () then
                iter (i, j)
              else
                split (i, j)
          and split (i: word, j: word): unit =
              let val mid = midpoint (i, j) in
                  ignore (par (fn () => splitCheckBoundsTokens (i, mid),
                               fn () => splitCheckBoundsTokens (mid, j)))
                end
          and iterCheckBounds (i: word, j: word): unit =
              if grain >= j - i then
                seqfor (i, j)
              else
                iter (i, j)
          and iter (i: word, j: word): unit =
              let val i' = i + grain
                  fun unstolen () = splitCheckBounds (i', j)
                  fun body () = for (i, i') step
                  fun spwn () = unstolen ()
                  fun seq () = iterCheckBounds (i', j)
                  fun sync ((), ()) = ()
              in
                sporkSamGive (body, spwn, seq, sync, unstolen)
              end
      in
        splitCheckBoundsTokens (i, j)
      end

  (* fun wparfor (grain: word) (i: word, j: word) (f: word -> unit): unit = *)
  (*     let fun seqfor (i, j) = *)
  (*             for (i, j) f *)
  (*         fun eagers (i, j) = *)
  (*             if grain >= j - i then *)
  (*               seqfor (i, j) *)
  (*             else if Scheduler.SporkJoin.noTokens () then *)
  (*               iter (i, j) *)
  (*             else *)
  (*               let val mid = midpoint (i, j) in *)
  (*                 par (fn () => eagers (i, mid), *)
  (*                      fn () => eagers (mid, j)); *)
  (*                 () *)
  (*               end *)
  (*         and iterCheck (i, j) = *)
  (*             if grain >= j - i then *)
  (*               seqfor (i, j) *)
  (*             else *)
  (*               iter (i, j) *)
  (*         and iter (i, j) = *)
  (*             let val i' = i + grain *)
  (*                 fun body () = seqfor (i, i') *)
  (*                 fun spwn () = eagers (i', j) *)
  (*                 fun seq () = iterCheck (i', j) *)
  (*                 fun sync ((), ()) = () *)
  (*                 fun unstolen () = eagers (i', j) *)
  (*             in *)
  (*               sporkSamGive (body, spwn, seq, sync, unstolen) *)
  (*             end *)
  (*     in *)
  (*       eagers (i, j) *)
  (*     end *)

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
                  fun body () = reduce (i, i') b step
                  fun spwn () = unstolen z
                  fun seq b' = iterCheckBounds b' (i', j)
                  val sync = merge
              in
                sporkSamGive (body, spwn, seq, sync, unstolen)
              end
      in
        splitCheckBoundsTokens z (i, j)
      end


  (*fun wsppareduce (grain: word) (i: word, j: word) (z: 'a) (step: word -> 'a) (smerge: 'a * 'a -> 'a) (pmerge: 'a * 'a -> 'a): 'a =
      let fun seqred (b: 'a) (i: word, j: word) =
              reduce (i, j) b (fn (i, a) => smerge (a, step i))
          fun eagers (b: 'a) (i: word, j: word): 'a =
              if grain >= j - i then
                seqred b (i, j)
              else if Scheduler.SporkJoin.noTokens () then
                iter b (i, j)
              else
                let val mid = midpoint (i, j) in
                  merge (par (fn () => eagers b (i, mid),
                              fn () => eagers z (mid, j)))
                end
          and iterCheck (b: 'a) (i: word, j: word): 'a =
              if grain >= j - i then
                seqred b (i, j)
              else
                iter b (i, j)
          and iter (b: 'a) (i: word, j: word): 'a =
              let val i' = i + grain
                  fun unstolen b' = eagers b' (i', j)
                  fun body () = reduce (i, i') b step
                  fun spwn () = unstolen z
                  fun seq b' = iterCheck b' (i', j)
                  val sync = merge
              in
                sporkSamGive (body, spwn, seq, sync, unstolen)
              end
      in
        eagers z (i, j)
      end*)


  (*fun wparfor (grain: word) (i: word, j: word) (f: word -> unit): unit =
      let fun seqfor (i, j) =
              for (i, j) f
          fun loop (i, j) =
              if Scheduler.SporkJoin.noTokens () then
                parfor_spork_one_body (i, j)
              else
                parfor_split (i, j)

          and parfor_spork_one_body (i, j) =
              if grain >= j - i then
                seqfor (i, j)
              else
                let
                  val i' = i + grain
                  fun body () = seqfor (i, i')
                  fun seq () = parfor_spork_one_body (i', j)
                  fun spwn () = loop (i', j)
                  fun sync ((), ()) = ()
                in
                  sporkGive (body, spwn, seq, sync)
                end
                  
          and parfor_split (i, j) =
              if grain >= j - i then
                seqfor (i, j)
              else
                let val mid = midpoint (i, j) in
                  par (fn () => loop (i, mid),
                       fn () => loop (mid, j));
                  ()
                end
      in
        loop (i, j)
      end*)

  (* ======================================================================= *)

  (* fun wpareduce_old (grain: word) (i: word, j: word) (z: 'a) (step: word * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a = *)
  (*     let fun loop (a: 'a, i: word, j: word): 'a = *)
  (*             let val i' = i + grain in *)
  (*               if i' >= j then *)
  (*                 reduce (i, j) a step *)
  (*               else *)
  (*                 sporkGive *)
  (*                   (fn () => reduce (i, i') a step, *)
  (*                    fn () => let val mid = midpoint (i', j) in *)
  (*                               merge (par (fn () => loop (z, i', mid), *)
  (*                                           fn () => loop (z, mid, j))) *)
  (*                             end, *)
  (*                    fn (a') => loop (a', i', j), *)
  (*                    merge) *)
  (*             end *)
  (*     in *)
  (*       loop (z, i, j) *)
  (*     end *)

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

  fun wparfor'' (grain: word) (i: word, j: word) (iter: word -> unit): unit =
      let fun seqfor (i: word, j: word): unit =
              for (i, j) iter
          fun loop (i: word, j: word): unit =
              if i + grain >= j then
                seqfor (i, j)
              else
                let fun innerLoop (i: word, j: word): unit =
                        let val i' = i + grain in
                          if i' >= j then
                            seqfor (i, j)
                          else
                            let fun body () = seqfor (i, i')
                                fun spwn () = loop (i', j)
                                fun seq () = innerLoop (i', j)
                                fun sync ((), ()) = ()
                            in
                              sporkGive (body, seq, spwn, sync)
                            end
                        end
                    val mid = midpoint (i, j)
                in
                  par (fn () => innerLoop (i, mid),
                       fn () => innerLoop (mid, j)); ()
                end
      in
        loop (i, j)
      end

  fun wpareduce'' (grain: word) (i: word, j: word) (z: 'a) (iter: word -> 'a) (merge: 'a * 'a -> 'a): 'a =
      let fun seqred (a: 'a, i: word, j: word): 'a =
              reduce (i, j) a (fn (i, a) => merge (a, iter i))
          fun loop (a: 'a) (i: word, j: word): 'a =
              if i + grain >= j then
                seqred (a, i, j)
              else
                let fun innerLoop (a: 'a) (i: word, j: word): 'a =
                        let val i' = i + grain in
                          if i' >= j then
                            seqred (a, i, j)
                          else
                            let fun body () = seqred (a, i, i')
                                fun spwn () = loop z (i', j)
                                fun seq (a) = innerLoop a (i', j)
                                fun sync (a1, a2) = merge (a1, a2)
                            in
                              sporkGive (body, spwn, seq, sync)
                            end
                        end
                    val mid = midpoint (i, j)
                in
                  merge (par (fn () => innerLoop a (i, mid),
                              fn () => innerLoop z (mid, j)))
                end
      in
        loop z (i, j)
      end

  fun parfor (grain: int) (i: int, j: int) (iter: int -> unit): unit =
      wparfor (i2w grain) (i2w i, i2w j) (iter o w2i)
  fun parfor_sam (grain: int) (i: int, j: int) (iter: int -> unit): unit =
      wparfor_sam (i2w grain) (i2w i, i2w j) (iter o w2i)
  fun pareduce (grain: int) (i: int, j: int) (b: 'a) (step: int * 'a -> 'a) (merge: 'a * 'a -> 'a): 'a =
      wpareduce (i2w grain) (i2w i, i2w j) b (fn (w, a) => step (w2i w, a)) merge
  fun pareduce' (grain: int) (i: int, j: int) (b: 'a) (iter: int -> 'a) (merge: 'a * 'a -> 'a): 'a =
      wpareduce' (i2w grain) (i2w i, i2w j) b (iter o w2i) merge
  fun pareduce'' (grain: int) (i: int, j: int) (b: 'a) (iter: int -> 'a) (merge: 'a * 'a -> 'a): 'a =
      wpareduce'' (i2w grain) (i2w i, i2w j) b (iter o w2i) merge
  fun pareduce_sam (grain: int) (i: int, j: int) (b: 'a) (iter: int -> 'a) (merge: 'a * 'a -> 'a): 'a =
      wpareduce_sam (i2w grain) (i2w i, i2w j) b (iter o w2i) merge

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
