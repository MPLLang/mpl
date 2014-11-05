functor Blocked (structure A: DENSEARG
                 structure D: DENSE where type m = real A.t A.t) : DENSE =
struct

  exception Blocked of string

  open A

  type v = real t
  type m = v t

  datatype tree = Leaf of v t
                (* dim of UL used in unwrap *)
                | Branch of { height : int, 
                              width : int,
                              ul : tree,
                              ur : tree, 
                              ll : tree,
                              lr : tree } 

  fun vva maxSeq (a, b) =
    let
      val def = ~1.0
    in
      tabulate maxSeq (fn i => sub (a, i) + sub (b, i))
               def (length a)
    end

  fun mma maxSeq (Branch {ul = ul1, ur = ur1, ll = ll1, lr = lr1, height, width},
                  Branch {ul = ul2, ur = ur2, ll = ll2, lr = lr2, ...}) =
      let
(*
        val () = print "A";
*)
        (* XXX use for better granularity *)
        fun inParallel (f, g) = (f (), g ())
        val ((ul, ur), (ll, lr)) =
            inParallel (fn () => inParallel (fn () => mma maxSeq (ul1, ul2),
                                             fn () => mma maxSeq (ur1, ur2)),
                        fn () => inParallel (fn () => mma maxSeq (ll1, ll2),
                                             fn () => mma maxSeq (lr1, lr2)))
      in
        Branch {ul = ul, ur = ur, ll = ll, lr = lr, height = height, width = width}
      end

    | mma maxSeq (Leaf m1, Leaf m2) =
      let 
(*
        val () = print "a";

        val () = print ("(" ^ (Int.toString (length m1)) 
                        ^ "x" ^ (Int.toString (length (sub (m1, 0)))) 
                        ^ ")+(" ^ (Int.toString (length m2))
                        ^ "x" ^ (Int.toString (length (sub (m2, 0)))) ^ ")")
*)
        val def = tabulate maxSeq (fn _ => ~1.0) ~1.0 0
      in
        Leaf (tabulate maxSeq (fn i => vva maxSeq (sub (m1, i), sub (m2, i)))
                       def (length m1))
      end
    | mma _ _ = raise Blocked "blocked matrices must have the same structure"

  (* XXX use second version for better granularity *)
  (* fun mma' maxSeq (f, g) () = mma maxSeq (inParallel (f, g)) *)
  fun mma' maxSeq (f, g) () = mma maxSeq (f (), g ()) 

  fun mmm' maxSeq (Branch {ul = ul1, ur = ur1, ll = ll1, lr = lr1, ...},
                   Branch {ul = ul2, ur = ur2, ll = ll2, lr = lr2, ...}) () =
      let (* val () = print "M" *)
        val ((ul, ur), (ll, lr)) = 
            (* PERF consider more Grey-like ordering? *)
            inParallel (fn () => inParallel (mma' maxSeq (mmm' maxSeq (ul1, ul2),
                                                          mmm' maxSeq (ur1, ll2)),
                                             mma' maxSeq (mmm' maxSeq (ul1, ur2),
                                                          mmm' maxSeq (ur1, lr2))),
                        fn () => inParallel (mma' maxSeq (mmm' maxSeq (ll1, ul2),
                                                          mmm' maxSeq (lr1, ll2)),
                                             mma' maxSeq (mmm' maxSeq (ll1, ur2),
                                                          mmm' maxSeq (lr1, lr2)
                                                          (* do a GC here if we 
                                                              are measuring space *)
                                                          before GC ())))

        fun dims (Leaf m) = (length m, length (sub (m, 0)))
          | dims (Branch { ul, ur, ll, ... }) = 
            let
              val (h, w) = dims ul
              val (h', w') = (#1 (dims ll), #2 (dims ur))
            in
              (h + h', w + w')
            end

        val (height, width) = dims ul
      in
        Branch {ul = ul, ur = ur, ll = ll, lr = lr, height = height, width = width}
      end

    | mmm' maxSeq (Leaf m1, Leaf m2) () = 
      let
(*
        val () = print "m"

        val () = print ("(" ^ (Int.toString (length m1)) 
                        ^ "x" ^ (Int.toString (length (sub (m1, 0)))) 
                        ^ ")*(" ^ (Int.toString (length (sub (m2, 0))))
                        ^ "x" ^ (Int.toString (length m2)) ^ ")")
*)
      in 
        Leaf (D.mmm maxSeq (m1, m2))
      end

    | mmm' _ (m1, m2) () = 
      let 
        val () = describeTree false m1
        val () = describeTree true m2
      in
        raise Blocked "blocked matrices must have the same structure"
      end

  and describeTree trans t =
      let
        val () = print "\n"

        fun dim (Leaf m) = 
              (length m, length (sub (m, 0)))

          | dim (Branch {ul, ur, ll, ...}) =
            let
              val (h, w) = dim ul
              val (h', w') = if trans then
                               (#1 (dim ur), #2 (dim ll))
                             else 
                               (#1 (dim ll), #2 (dim ur))
            in
              (h + h', w + w')
            end

        fun pr prefix (t as Leaf m) = 
            let 
              val (h, w) = dim t
              fun vtos v = foldArray 100000 (fn (x, s) => s ^ x ^ ", ") Real.toString "" v
            in
              print (prefix ^ (Int.toString h) ^ " x " ^ (Int.toString w) ^ "\n") (* ;
              print 
                  (foldArray 100000 (fn (s, r) => r ^ prefix ^ "  " ^ s ^ "\n") vtos "" m) *)
            end
          | pr prefix (t as Branch { ul, ur, ll, lr, height, width }) =
            let 
              val (h, w) = dim t
            in
              print (prefix ^ "(" ^ (Int.toString h) ^ " x " ^ (Int.toString w) ^ ") -->\n");
              pr (prefix ^ "  ") ul;
              pr (prefix ^ "  ") ur;
              pr (prefix ^ "  ") ll;
              pr (prefix ^ "  ") lr
            end
      in
        pr "" t
      end

  fun mmm maxSeq (m1, m2) =
      let 
        fun wrap trans m depth (x, y) (height, width) = 
            if depth = 0
            then
              let 
                val def = tabulate maxSeq (fn _ => ~1.0) ~1.0 0
                val k = if y + width > length (sub (m, x)) then NONE else SOME width
                val m = tabulate maxSeq (fn i => slice (sub (m, x + i), y, k))
                                 def (if x + height > length m then (length m) - x else height)
              in
                Leaf m
              end
            else
              let
                (* round up *)
                val height' = height div 2 + (if height mod 2 > 0 then 1 else 0)
                val width' = width div 2 + (if width mod 2 > 0 then 1 else 0)
                val depth' = depth - 1
              in
                if trans then
                  Branch {ul = wrap trans m depth' (x, y)                    (height', width'), 
                          ur = wrap trans m depth' (x + height', y)          (height - height', width'),
                          ll = wrap trans m depth' (x, y + width')           (height', width - width'),
                          lr = wrap trans m depth' (x + height', y + width') (height - height', width - width'),
                          width = width', height = height'}
                else
                  Branch {ul = wrap trans m depth' (x, y)                    (height', width'),
                          ur = wrap trans m depth' (x, y + width')           (height', width - width'),
                          ll = wrap trans m depth' (x + height', y)          (height - height', width'),
                          lr = wrap trans m depth' (x + height', y + width') (height - height', width - width'),
                          width = width', height = height'}
              end

        fun unwrap m len = 
            let
              val debug = ref false

              val def = tabulate maxSeq (fn _ => ~1.0) ~1.0 0
              fun mySub (Leaf m) (x, y) = sub (sub (m, x), y)
                | mySub (Branch {ul, ur, ll, lr, height, width}) (x, y) =
                  if x < height then
                    if y < width then
                      mySub ul (x, y)
                    else
                      mySub ur (x, y - width)
                  else
                    if y < width then
                      mySub ll (x - height, y)
                    else
                      mySub lr (x - height, y - width)                         
            in
              tabulate maxSeq (fn i => tabulate maxSeq (fn j => (mySub m (i, j))
                                                           (* handle Subscript => 
                                                                  (print ("subscript! " ^ (Int.toString i) ^ ", " ^ (Int.toString j) ^ "\n");

                                                                   raise Blocked "subscript") *)
) ~1.0 len)
                       def len
            end

        fun computeDepth x =
            if x <= maxSeq then 0
            else 1 + (computeDepth ((x div 2) + (if x mod 2 > 0 then 1 else 0)))
        val depth = computeDepth (Int.max (length m1, length (sub (m1, 0))))
            
        val () = print "starting wrap..."
        val (m1', m2') = (wrap false m1 depth (0, 0) (length m1, length (sub (m1, 0))),
                          wrap true m2 depth (0, 0) (length m2, length  (sub (m2, 0))))

(*
        val () = print "m1 = \n"
        val () = describeTree false m1'
        val () = print "m2 = \n"
        val () = describeTree true m2'
*)
        val () = print "finished wrap..."
        val m' = mmm' maxSeq (m1', m2') ()
        val () = print "finished mmm..."
(*
        val () = print "m' = \n"
        val () = describeTree false m'
*)
      in
        unwrap m' (length m1) before print "finished unwrap..."
      end

end
