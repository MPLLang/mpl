structure MLtonParallelForkJoin :> MLTON_PARALLEL_FORKJOIN = 
struct

  structure B = MLtonParallelBasic
  structure V = MLtonParallelSyncVarCapture
  structure T = MLtonThread

  datatype 'a result = 
     Finished of 'a
   | Raised of exn

  exception Impossible
  exception Impossible2

  val fetch_and_add = _import "Parallel_fetchAndAdd" : int ref * int -> int;

  fun forkLat gl (f, g) =
      let fun doFork k =
              let val left = ref NONE
                  val right = ref NONE
                  val trr = ref ""
                  val join = ref 0
                  val doRight = ref 0
                  (* Closure used to run the right-hand side... but
                     only in the case where that code is run in parallel. *)
                  (* XXX  TODO Handle exceptions *)
                  fun rightside () =
                      (let val r = fetch_and_add (doRight, 1)
                           val _ = if r > 0 then
                                       (print "BAD!"; raise Impossible)
                                   else ()
                           val _ = print "right stolen\n"
                           (* val _ = print ("Doing " ^ (!trr) ^ "\n") *)
                           val res = g () in
                           (case !right of
                                NONE => ()
                              | SOME _ => raise Impossible2);
                           right := SOME res;
                           if fetch_and_add (join, 1) = 1 then
                               (* left side already finished; do the join *)
                               case !left of
                                   NONE => raise Impossible
                                 | SOME l => ((* print "resuming on right\n"; *)
                                              B.resume (k, (l, res));
                                              B.return ())
                           else
                               B.return ()
                       end)
                  fun leftside () =
                      (let val res = f () in
                           (case !left of
                                NONE => ()
                              | SOME _ => raise Impossible2);
                           left := SOME res;
                           if fetch_and_add (join, 1) = 1 then
                               (* right side already finished; do the join *)
                               case !right of
                                   NONE => raise Impossible
                                 | SOME r => ((* print "resuming on left\n"; *)
                                              B.resume (k, (res, r));
                                              B.return ())
                           else
                               B.return ()
                       end)
                  (* Offer the right side to any processor that wants it *)
                  val tr = B.addRightLat (gl, rightside) (* might suspend *)
                  val tl = B.addRightLat (gl, leftside)
              in
                  trr := (B.stringOfToken tr)
              end
      in
        B.capture doFork
      end

  fun fork (f, g) = forkLat false (f, g)

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
