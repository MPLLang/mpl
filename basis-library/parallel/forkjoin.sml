structure MLtonParallelForkJoin :> MLTON_PARALLEL_FORKJOIN =
struct

  structure B = MLtonParallelBasic
  structure HH = MLtonHM.HierarchicalHeap
  structure V = MLtonParallelSyncVarCapture

  datatype 'a result =
     Finished of 'a * HH.t
   | Raised of exn * HH.t

   local
      fun doFork (f, g) =
          let
              (* Used to hold the result of the right-hand side in the case where
          that code is executed in parallel. *)
              val var = V.empty ()
              (* Closure used to run the right-hand side... but only in the case
          where that code is run in parallel. *)
              fun rightside () =
                  let
                      val hh = HH.get ()
                  in
                      V.write (var,
                               Finished (g (), hh)
                               handle e => Raised (e, hh));
                      B.return ()
                  end

              (* Offer the right side to any processor that wants it *)
              val t = B.addRight rightside (* might suspend *)
              (* Run the left side *)
              val a = f ()
                      (* XXX Do we need to execute g in the case where f raises? *)
                      handle e => (ignore (B.remove t); B.yield (); raise e)
              (* Try to retract our offer -- if successful, run the right side
          ourselves. *)
              val b =
                  if B.remove t
                  then
                      (*
                       * no need to yield since we expect this work to be the
                       * next thing in the queue
                       *)
                      g ()
                      handle e => (B.yield (); raise e)
                  else
                      (* must have been stolen, so I have a heap to merge *)
                      case V.read var
                       of (_, Finished (b, childHH)) =>
                          (HH.mergeIntoParent childHH;
                           b)
                        | (_, Raised (e, childHH)) =>
                          (HH.mergeIntoParent childHH;
                           B.yield ();
                           raise e)
          in
              B.yield ();
              (a, b)
          end
  in
      fun fork functions =
          let
              val _ = MLtonHM.enterGlobalHeap ()
              val result = doFork functions
              val _ = MLtonHM.exitGlobalHeap ()
          in
              result
          end
  end

  local
      fun doReduce maxSeq f g u n =
          let
              val () = if maxSeq < 1 then raise B.Parallel "maxSeq must be at least 1" else ()

              fun wrap i l () =
                  if l <= maxSeq then
                      let
                          val stop = i + l
                          fun loop j v = if j = stop then v
                                         else loop (j + 1) (f (v, g j))
                      in
                          loop i u
                      end
                  else
                      let
                          val l' = l div 2
                      in
                          f (fork (wrap i l',
                                   wrap (i + l') (l - l')))
                      end
          in
              wrap 0 n ()
          end
  in
      fun reduce maxSeq f g u n =
          let
              val _ = MLtonHM.enterGlobalHeap ()
              val result = doReduce maxSeq f g u n
              val _ = MLtonHM.exitGlobalHeap ()
          in
              result
          end
  end

  local
      fun doReduce' maxSeq (g : int -> unit) n =
          let
              val () = if maxSeq < 1 then raise B.Parallel "maxSeq must be at least 1" else ()

              fun wrap i l () =
                  if l <= maxSeq then
                      let
                          val stop = i + l
                          fun loop j = if j = stop then ()
                                       else (g j; loop (j + 1))
                      in
                          loop i
                      end
                  else
                      let
                          val l' = l div 2
                      in
                          ignore (fork (wrap i l',
                                        wrap (i + l') (l - l')))
                      end
          in
              wrap 0 n ()
          end
  in
      fun reduce' maxSeq g n =
          let
              val _ = MLtonHM.enterGlobalHeap ()
              val result = doReduce' maxSeq g n
              val _ = MLtonHM.exitGlobalHeap ()
          in
              result
          end
  end

end
