
structure Priority :> PRIORITY =
struct

    exception InvariantViolated of string
    exception Uninitialized
    exception AlreadyInitialized
    exception InvalidArgument

    structure A = Array
    structure A2 = Array2
    structure V = Vector

    (* Index, postorder *)
    type t = int * (int ref) * (string option)

    val top_prio = (0, ref (~1), SOME "top")
    fun top _ = top_prio
    val bot = (1, ref (~1), SOME "bot")

    val numberOfPrios = ref 2
    val ltMat : bool A2.array ref =
        ref (A2.fromList [[false, false], [false, false]])
    val byid : t V.vector ref = ref (V.fromList [top (), bot])
    val bypostorder : t A.array ref = ref (A.fromList [])
    val initialized = ref false
    val checked = ref false

    val distFunc = ref (fn _ => top ())

    fun count () = !numberOfPrios

    fun eprint s = print s
    fun print _ = ()

    (* Functions must be called in this order:
     * new (any number of times)
     * init
     * new_lessthan (any number of times)
     * check
     *
     * After calling check, functions may be called in this order:
     * ( (new*, init)* + (new_lessthan*, check)* )*
     *)

    fun get (p1: int) (p2 : int) : bool =
        A2.sub (!ltMat, p1, p2)
        handle Subscript => (print "get"; raise Subscript)

    fun plt (((p1, _, _): t), ((p2, _, _): t)) : bool =
        if not (!checked) then raise Uninitialized
        else
            get p1 p2

    fun ple (((p1, _, _): t), ((p2, _, _): t)) : bool =
        if not (!checked) then raise Uninitialized
        else
            p1 = p2 orelse get p1 p2

    fun pe (((p1, _, _): t), ((p2, _, _): t)) : bool =
        if not (!checked) then raise Uninitialized
        else
            p1 = p2

    fun set p1 p2 (v: bool) : unit =
        A2.update (!ltMat, p1, p2, v)

    val () = set 1 0 true

    fun warshall (k, i, j) : unit =
        let val n = !numberOfPrios
        in
            if k >= n then ()
            else if i >= n then warshall (k + 1, 0, 0)
            else if j >= n then warshall (k, i + 1, 0)
            else
                if ((i = k orelse get i k) andalso get k j) orelse
                   (get i k andalso (k = j orelse get k j))
                then
                    if i = j then
                        raise (InvariantViolated "priority graph is cyclic")
                    else
                        (set i j true; ())
                else
                    ()
        end

    fun getNbrs p1 =
        List.filter (fn j => get p1 j)
                    (List.tabulate (!numberOfPrios, fn n => n))

    fun dfs () : unit =
        let val po = ref 0
            fun dfs_int (pi, ipo, s) =
                if !ipo <> ~1 then
                    (* already visited *)
                    ()
                else
                    (print ((Int.toString pi) ^ "\n");
                     List.app dfs_int (List.map (fn i => V.sub (!byid, i))
                                                (getNbrs pi));
                     ipo := !po;
                     A.update (!bypostorder, !po, (pi, ipo, s));
                     print ("post " ^ (Int.toString pi) ^ "\n");
                     po := !po + 1)
        in
            bypostorder := A.tabulate (count (), fn _ => (~1, ref (~1), SOME "dummy"));
            dfs_int bot;
            V.app dfs_int (!byid)
        end

    fun init () : unit =
        let val n' = !numberOfPrios
            val n = A2.nCols (!ltMat)
        in
            ltMat := A2.tabulate A2.RowMajor
                                (n', n', fn (i, j) =>
                                            if i < n andalso j < n then
                                                A2.sub (!ltMat, i, j)
                                            else
                                                false
                                );
            initialized := true;
            ()
        end


    fun new () : t =
        let val n = !numberOfPrios
            val po = ref ~1
        in
            numberOfPrios := n + 1;
            initialized := false;
            byid := V.tabulate (n + 1, fn i => if i < n then V.sub (!byid, i)
                                               else (n, po, NONE));
            (n, po, NONE)
        end
        handle Subscript => (print "new"; raise Subscript)

    fun new_lessthan ((p1, _, _): t) ((p2, _, _): t) : unit =
        if not (!initialized) then raise Uninitialized
        else
            if p1 = p2 then raise (InvariantViolated "priority graph is cyclic")
            else
                (set p1 p2 true;
                 ())

    fun check () : unit =
        if not (!initialized) then raise Uninitialized
        else if A2.nCols (!ltMat) <> !numberOfPrios
        then raise (InvariantViolated "matrix dimension doesn't match no of priorities")
        else
            (V.app (fn p => new_lessthan p top_prio);
             V.app (fn p => new_lessthan bot p);
             dfs ();
             warshall (0, 0, 0);
             checked := true)

    fun toInt ((_, po, _): t) : int =
        ((count ()) - !po
         before print ("po: " ^ (Int.toString (!po)) ^ "\n"))
    fun fromInt (n: int) : t =
        let val np = count ()
            val _ = print ("np = " ^ (Int.toString np) ^ "\n")
            val po = np - n
        in
            if n <= 0 orelse n > np then
                (print ((Int.toString n) ^"\n");
                raise InvalidArgument)
            else
                (
                  print ((Int.toString po) ^"\n");
                  A.sub (!bypostorder, po)
                )
        end
        handle Subscript => (print "fromInt"; raise Subscript)

    fun toString (p as (_, _, s) : t) : string =
        case s of
            NONE => Int.toString (toInt p)
          | SOME s' => s'

    fun installDist (f: t -> int) =
        let val raw = V.tabulate (count (),
                                  fn i => let val r = fromInt (i + 1)
                                          in
                                              (r, f r)
                                          end)
            val sum = Real.fromInt (Vector.foldl (fn ((_, n), a) => a + n)
                                                 0 raw)
            val cumulative =
                Vector.foldl (fn ((r, n), (a, l)) =>
                                 let val prob = Real./ (Real.fromInt n, sum)
                                 in
                                     (a + prob, (r, 1.0 - a)::l)
                                 end)
                             (0.0, [])
                             raw
            fun find l x =
                case l of
                    [] => raise InvalidArgument
                  | (r, p)::t => if x < p then r
                                 else find t x
        in
            distFunc := (find (#2 cumulative))
        end

    fun chooseFromDist x =
        let (* val x = UsefulRandom.rand01 () *)
        in
            (!distFunc) x
        end

    fun next (r: t) =
        let val po = toInt r
        in
            if po = 1 then top ()
            else
                fromInt (po - 1)
        end
end
