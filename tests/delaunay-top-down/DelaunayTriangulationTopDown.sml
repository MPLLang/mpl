functor DelaunayTriangulationTopDown (structure R: REAL structure I: INTEGER) =
struct

  fun par3 (f1, f2, f3) =
    let val (a, (b, c)) = ForkJoin.par (f1, fn _ => ForkJoin.par (f2, f3))
    in (a, b, c)
    end

  type id = I.int

  fun r (x: real) : R.real =
    R.fromLarge IEEEReal.TO_NEAREST (Real.toLarge x)

  fun ii x = I.fromInt x

  structure Point =
  struct
    datatype t = T of {id: id, x: R.real, y: R.real}

    fun x (T p) = #x p
    fun y (T p) = #y p
    fun id (T p) = #id p
    fun id_cmp (p1, p2) =
      I.compare (id p1, id p2)
    fun id_less_than (p1, p2) =
      I.< (id p1, id p2)

    type vec = LargeReal.real * LargeReal.real * LargeReal.real

    fun cross ((x1, y1, z1): vec, (x2, y2, z2): vec) =
      (y1 * z2 - z1 * y2, z1 * x2 - x1 * z2, x1 * y2 - y1 * x2)

    fun dot ((x1, y1, z1): vec, (x2, y2, z2): vec) =
      x1 * x2 + y1 * y2 + z1 * z2

    fun project (d: t) (p: t) =
      let
        val px = R.toLarge (R.- (x p, x d))
        val py = R.toLarge (R.- (y p, y d))
      in
        (px, py, LargeReal.+ (LargeReal.* (px, px), LargeReal.* (py, py)))
      end

    fun in_circle (a: t, b: t, d: t) =
      let val cp = cross (project d a, project d b)
      in fn c => dot (cp, project d c) > 0.0
      end
  end

  type tri = id * id * id
  val dummy_tri = (ii ~1, ii ~1, ii ~1)

  type edge = id * id
  val dummy_edge = (ii ~1, ii ~1)

  fun itow64 i =
    Word64.fromLarge (LargeWord.fromInt (I.toInt i))

  structure EH =
    HashTable
      (struct
         type t = edge
         val equal = op=
         val default = dummy_edge
         fun hash (i1, i2) =
           Word64.toIntX (Word64.xorb
             (Util.hash64 (itow64 i1), Util.hash64_2 (itow64 i2)))
       end)

  structure TH =
    HashTable
      (struct
         type t = tri
         val equal = op=
         val default = dummy_tri
         fun hash (i1, i2, i3) =
           Word64.toIntX
             (Word64.xorb
                ( Util.hash64 (itow64 i1)
                , Word64.xorb (Util.hash64_2 (itow64 i2), Util.hash64_2
                    (Util.hash64_2 (itow64 i3)))
                ))
       end)


  type triangle = {tri: tri, conflicts: Point.t Seq.t}


  fun filter_points points (t1: triangle, t2: triangle, t: tri) =
    let
      val a = Merge.merge Point.id_cmp (#conflicts t1, #conflicts t2)
      val an = Seq.length a

      fun lookup_point id =
        Seq.nth points (I.toInt id)

      val is_in_circle = Point.in_circle
        (lookup_point (#1 t), lookup_point (#2 t), lookup_point (#3 t))

      fun same_id i j =
        Point.id_cmp (Seq.nth a i, Seq.nth a j) = EQUAL

      fun keep i =
        (i <> 0) andalso not (same_id i (i - 1))
        andalso
        ((i + 1 < an andalso same_id i (i + 1))
         orelse is_in_circle (Seq.nth a i))
    in
      ArraySlice.full (SeqBasis.filter 2000 (0, an) (Seq.nth a) keep)
    end


  (* ~2/3 max load with a bit of slop *)
  fun sloppy_capacity max_expected_elems =
    100 + (max_expected_elems * 3) div 2


  fun triangulate (points: Point.t Seq.t) =
    let
      val n = Seq.length points

      fun earliest ({tri, conflicts}: triangle) =
        if Seq.length conflicts = 0 then ii n
        else Point.id (Seq.nth conflicts 0)

      val edges = EH.make
        { default = {tri = dummy_tri, conflicts = Seq.fromList []}
        , capacity = sloppy_capacity (6 * n)
        }

      val mesh = TH.make {default = (), capacity = sloppy_capacity (2 * n)}

      (* enclosing triangle *)
      val p0 = Point.T {id = ii n, x = r 0.0, y = r 100.0}
      val p1 = Point.T {id = ii (n + 1), x = r 100.0, y = r ~100.0}
      val p2 = Point.T {id = ii (n + 2), x = r ~100.0, y = r ~100.0}
      val enclosing_t =
        {tri = (ii n, ii (n + 1), ii (n + 2)), conflicts = points}

      val all_points = Seq.append (points, Seq.fromList [p0, p1, p2])

      fun process_edge (t1: triangle, e: edge, t2: triangle) =
        if Seq.length (#conflicts t1) = 0 andalso Seq.length (#conflicts t2) = 0 then
          (TH.insert mesh (#tri t1, ()); TH.insert mesh (#tri t2, ()); ())
        else if earliest t1 = earliest t2 then
          ()
        else
          let
            val (t1, e, t2) =
              if I.<= (earliest t1, earliest t2) then (t1, e, t2)
              else (t2, (#2 e, #1 e), t1)

            val p = earliest t1
            val t = (#1 e, #2 e, p)
            val t1 = {tri = t, conflicts = filter_points all_points (t1, t2, t)}
          in
            par3
              ( fn _ => check_edge ((p, #1 e), t1)
              , fn _ => check_edge ((#2 e, p), t1)
              , fn _ => process_edge (t1, e, t2)
              );
            ()
          end


      and check_edge (e: edge, tp: triangle) =
        let
          val key = if I.< (#1 e, #2 e) then e else (#2 e, #1 e)
        in
          if EH.insert edges (key, tp) then
            ()
          else
            case EH.remove edges key of
              NONE => raise Fail "impossible?"
            | SOME tt => process_edge (tp, e, tt)
        end


      val t = enclosing_t
      val te = {tri = dummy_tri, conflicts = Seq.empty ()}
      val _ =
        par3
          ( fn _ => process_edge (t, (ii n, ii (n + 1)), te)
          , fn _ => process_edge (t, (ii (n + 1), ii (n + 2)), te)
          , fn _ => process_edge (t, (ii (n + 2), ii n), te)
          )
    in
      TH.keys mesh
    end

end
