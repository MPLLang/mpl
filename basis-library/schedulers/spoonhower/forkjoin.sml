structure MLtonParallelForkJoin :> MLTON_PARALLEL_FORKJOIN =
struct

  structure B = MLtonParallelBasic
  structure V = MLtonParallelSyncVarCapture

  datatype 'a result =
     Finished of 'a
   | Raised of exn

  fun fork (f, g) =
      let
        (* Used to hold the result of the right-hand side in the case where
          that code is executed in parallel. *)
        val var = V.empty ()
        (* Closure used to run the right-hand side... but only in the case
          where that code is run in parallel. *)
        fun rightside () = (V.write (var, Finished (g ())
                                          handle e => Raised e);
                            B.return ())

        (* Offer the right side to any processor that wants it *)
        val t = B.addRight rightside (* might suspend *)
        (* Run the left side *)
        val a = f ()
            (* XXX Do we need to execute g in the case where f raises? *)
            handle e => (ignore (B.remove t); B.yield (); raise e)
        (* Try to retract our offer -- if successful, run the right side
          ourselves. *)
        val b = if B.remove t then
                 (* no need to yield since we expect this work to be the next thing
                    in the queue *)
                  g ()
                  handle e => (B.yield (); raise e)
                else
                  case V.read var of (_, Finished b) => b
                                   | (_, Raised e) => (B.yield (); raise e)
      in
        B.yield ();
        (a, b)
      end

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

  fun for (i, j) f = if i = j then () else (f i; for (i+1, j) f)

  fun parfor grain (i, j) f =
    let val n = j - i
    in if n <= grain
       then for (i, j) f
       else ( fork ( fn _ => parfor grain (i, i + n div 2) f
                   , fn _ => parfor grain (i + n div 2, j) f
                   )
            ; ()
            )
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

  fun communicate () = ()

  fun getIdleTime _ = Time.zeroTime

end
