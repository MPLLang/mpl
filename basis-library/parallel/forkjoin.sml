structure MLtonParallelForkJoin :> MLTON_PARALLEL_FORKJOIN =
struct

  structure B = MLtonParallelBasic
  structure HM = MLtonHM
  structure HH = HM.HierarchicalHeap
  structure I = MLtonParallelInternal
  structure V = MLtonParallelSyncVar

  exception ShouldNotHappen

  datatype 'a result = Finished of 'a ref HH.t
                     | Raised of exn ref HH.t

  val dbgmsg = (* I.dbgmsg *) fn _ => ()

  val numTasks = Array.array (Word32.toInt I.numberOfProcessors, 0)
  fun incrTask n =
      let
        val p = Word32.toInt (I.processorNumber ())
      in
        Array.update (numTasks, p, Array.sub (numTasks, p) + n)
      end

  (* RAM_NOTE: How to handle exceptions and heaps? *)
  local
    fun evaluateFunction f (exceptionHandler: unit -> unit) =
        let
          (* RAM_NOTE: I need an uncounted enter/exit heap *)
          val result = (SOME(f ()), NONE)
                       (* SPOONHOWER_NOTE Do we need to execute g in the case where f raises? *)
                       handle e => (NONE, SOME e)
        in
          case result
           of (SOME(r), NONE) => r
            | (NONE, SOME(e)) => (exceptionHandler ();
                                  raise e)
            | _ => raise ShouldNotHappen
        end
  in
    fun fork (f, g) =
        let
          (* Make sure calling thread is set to use hierarchical heaps *)
          val () = (HM.enterGlobalHeap ();
                    HH.setUseHierarchicalHeap true;
                    ignore (HH.get ());
                    HM.exitGlobalHeap ())

          (* increment task by two *)
          val () = incrTask 2

          (* make sure a hh is set *)
          val hh = HH.get ()
          val level = HH.getLevel hh
          val () = dbgmsg ("(" ^ (Int.toString level) ^ "): Called fork")

          val var = V.empty ()

          val rightside =
           fn () =>
              let
                (*
                 * RAM_NOTE: Is there a way to force a ref without
                 * explicitly putting it in one?
                 *)
                val hh = HH.get ()
                val r =
                    let
                      val r = ref (g ())
                    in
                      Finished (HH.setReturnValue (hh, r))
                    end
                    handle e =>
                           let
                             val e = ref e
                           in
                             Raised (HH.setReturnValue (hh, e))
                           end
              in
                V.write (var, r);
                B.return ()
              end
              handle B.Parallel msg =>
                     (print (msg ^ "\n");
                      raise B.Parallel msg)

          (* Increment level for chunks allocated by 'f' and 'g' *)
          val () = HH.setLevel (hh, level + 1)

          (* Offer the right side to any processor that wants it *)
          val t = B.addRight (rightside, level)(* might suspend *)

          (* Run the left side in the hierarchical heap *)
          val () = dbgmsg ("(" ^ (Int.toString level) ^ "): START left")
          val a = evaluateFunction f (fn () => (ignore (B.remove t)))
          val () = dbgmsg ("(" ^ (Int.toString level) ^ "): END left")

          (*
           * Try to retract our offer -- if successful, run the right side
           * ourselves.
           *)
          val () = dbgmsg ("(" ^ (Int.toString level) ^ "): START right")
          val b =
              if B.remove t
              then
                (*
                 * no need to yield since we expect this work to be the
                 * next thing in the queue
                 *)
                (dbgmsg ("(" ^ (Int.toString level) ^ "): right not stolen");
                 evaluateFunction g (fn () => ()))
              else
                (* must have been stolen, so I have a heap to merge *)
                (dbgmsg ("(" ^ (Int.toString level) ^ "): right stolen");
                 case V.read var
                  of (_, Finished (childHH)) =>
                     let
                       val b = HH.mergeIntoParentAndGetReturnValue childHH
                     in
                       case b
                        of NONE => raise ShouldNotHappen
                         | SOME b => !b
                     end
                   | (_, Raised (childHH)) =>
                     (*
                      * RAM_NOTE: This definitely needs to be handled
                      * differently wrt HH operations later.
                      *)
                     let
                       val e = HH.mergeIntoParentAndGetReturnValue childHH;
                     in
                       case e
                        of NONE => raise ShouldNotHappen
                         | SOME e => raise !e
                     end)

          val () = dbgmsg ("(" ^ (Int.toString level) ^ "): END right")

          (*
           * At this point, g is done and merged, as if it was performed
           * serially
           *)
          val () = HH.promoteChunks hh

          (* Reset level *)
          val () = HH.setLevel (hh, level)
        in
          (a, b)
        end
  end

  fun getNumTasks () = Array.foldl (op+) 0 numTasks

  fun reduce maxSeq f g u n =
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

  fun reduce' maxSeq (g : int -> unit) n =
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
end
