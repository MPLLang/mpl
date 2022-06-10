structure ForkJoin :> FORK_JOIN =
   struct

      val pcall = _prim "PCall": ('a -> 'b) * 'a * ('b -> 'c) * ('b -> 'c) * ('d -> 'e) * 'd -> 'c;

      fun fork (f, g) =
         pcall (fn () => Result.result f, (),
                fn fres => (Result.extractResult fres, g ()),
                fn fres => (Result.extractResult fres, g ()),
                fn () => Primitive.MLton.bug "pcall/parr", ())

      fun par (f, g) = fork (f, g)

      fun for (i, j) f = if i >= j then () else (f i; for (i+1, j) f)

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
               if ArrayExtra.Raw.uninitIsNop a
                  then ()
                  else parfor 10000 (0, n) (fn i => ArrayExtra.Raw.unsafeUninit (a, i))
         in
            ArrayExtra.Raw.unsafeToArray a
         end
   end
