structure TreeSort =
struct
                                      (* length LHS*) (* RHS *)
  datatype t = Leaf of real list | Branch of int * t * int * t

end

functor TreeSortArg (val fork : (unit -> 'a) * (unit -> 'b) -> ('a * 'b)
                     type 'a future
                     val future : (unit -> 'a) -> 'a future
                     val touch : 'a future -> 'a) =
struct

  datatype t = datatype TreeSort.t

  exception Sort

  val fork = fork
  type 'a future = 'a future
  val future = future
  val touch = touch

  fun fromList maxSeq a = 
      let
        fun loop a = 
            let
              val len = length a
            in
              if case maxSeq of NONE => true
                              | SOME maxSeq => len < maxSeq 
              then (len, Leaf a)
              else
                let
                  val l = len div 2
                  val (b, c) = (List.take (a, l), 
                                List.drop (a, l))
                  val ((l, t), (m, u)) = fork (fn () => loop b, 
                                               fn () => loop c)
                in
                  (l + m, Branch (l, t, m, u))
                end
            end
      in
        #2 (loop a)
      end

  fun toList (Leaf a) = a
    | toList (Branch (_, t, _, u)) = (toList t) @ (toList u)

  val empty = Leaf nil
  fun singleton x = Leaf [x]

  fun length (Leaf a) = List.length a
    | length (Branch (l, _, m, _)) = l + m

  fun concat bs =
      let
        fun loop nil = (0, Leaf nil)
          | loop [a] = (length a, a)
          | loop bs = 
            let
              val l = List.length bs div 2
              val (cs, ds) = (List.take (bs, l), 
                              List.drop (bs, l))
(*
              val ((l, t), (m, u)) = (fork (fn () => loop cs,
                                            fn () => loop ds))
*)
              val ((l, t), (m, u)) = (loop cs, loop ds)
            in
              if l = 0 then (m, u)
              else if m = 0 then (l, t)
              else (l + m, Branch (l, t, m, u))
            end
      in
        #2 (loop bs)
      end

  fun append (t, Leaf nil) = t
    | append (Leaf nil, u) = u
    | append (t as Branch (l, _, m, _), 
              u as Branch (n, _, p, _)) = Branch (l + m, t, n + p, u)
    | append _ = raise Sort (* impossible *)

  fun foldli f y t =
      let
        fun oloop i y (Leaf a) = 
            let 
              fun iloop nil i y = y
                | iloop (x::xs) i y = iloop xs (i + 1) (f (i, x, y))
            in
              iloop a i y
            end
          | oloop i y (Branch (l, t, _, u)) =
            let
              val y = oloop i y t
            in
              oloop (i + l) y u
            end
      in
        oloop 0 y t
      end


  fun unfoldi maxSeq (n, b, f) =
      let
        fun loop (i, b, a) =
            if i = n then (a, b) else
            let 
              val (x, b) = f (i, b)
            in
              loop (i + 1, b, x :: a)
            end
        val (a, b) = loop (0, b, nil)
      in
        (fromList maxSeq (rev a), b)
      end

(* XXX use maxseq? *)
  fun filter maxSeq f t = 
      let 
        fun myFilter nil (c, acc) = (c, rev acc)
          | myFilter (x::xs) (c, acc) = if f x then myFilter xs (c + 1, x::acc)
                                        else myFilter xs (c, acc)

        fun loop _ (Leaf a) = 
            let 
              val (c, a) = myFilter a (0, nil)
            in
              (c, Leaf a)
            end
          | loop depth (Branch (l, t, m, u)) = 
            let
              val ((l, t), (m, u)) = 
                  if depth = 0 then (loop 0 t, loop 0 u)
                  else fork (fn () => loop (depth - 1) t,
                             fn () => loop (depth - 1) u)
            in
              if l = 0 then (m, u)
              else if m = 0 then (l, t)
              else (l + m, Branch (l, t, m, u))
            end

        val maxDepth = 3

      in
        #2 (loop maxDepth t)
      end

  (* PERF could use less randomness *)
  fun pivot (Leaf nil) = raise Sort
    | pivot (Leaf a) = List.nth (a, Random.randomInt (List.length a))
    | pivot (Branch (l, t, m, u)) = 
      if l = 0 then pivot u
      else if m = 0 then pivot t
      else if Random.randomBool () then pivot t 
      else pivot u

  fun sub (Leaf a, i) = List.nth (a, i)
    | sub (Branch (l, t, _, u), i) =
      if i < l then sub (t, i)
      else sub (u, i - l)

  fun update _ = raise Sort
  fun copy _ = raise Sort

  fun slice _ = raise Sort

(*
  fun slice (Leaf a, i, NONE) = Leaf (List.drop (a, i))
    | slice (Leaf a, i, SOME j) = Leaf (List.take (List.drop (a, i), j))
    | slice (Branch (t, u), i, k) = 
*)

  fun halve (Leaf a) = 
      let 
        val l = List.length a div 2
      in
        (Leaf (List.take (a, l)), Leaf (List.drop (a, l)))
      end
    | halve (Branch (l, t, m, u)) =
      let
      in
        (t, u)
      end

  fun printArg (Leaf a) = print ("[ " ^ (foldr (fn (x, s) => Real.toString x
                                                             ^ ", " ^ s) " ]" a))
    | printArg (Branch (_, t, _, u)) = (print "[ "; printArg t; print " "; printArg u; print " ]")

  fun GC () = if !MainUtils.explicitGC then MLton.GC.collect () else ()

end

structure Seq = 
struct
  fun fork (f, g) = (f (), g ()) 
  type 'a future = 'a
  fun future g = g ()
  fun touch f = f
end
structure ParNoDelay = 
struct 
  structure F = MLton.Parallel.FutureSuspend
  val fork = MLton.Parallel.ForkJoin.fork 
  type 'a future = 'a F.t
  val future = F.future
  val touch = F.touch
(*
  val fork = fn fg => (print "fork!\n"; fork fg)
  val future = fn g => (print "future!\n"; future g)
  val touch = fn f => (print "touch!\n"; touch f)
*)
end
structure ParMaybeDelay = 
struct 
  structure F = MLton.Parallel.FutureSuspendMaybeDelay
  val fork = MLton.Parallel.ForkJoin.fork 
  type 'a future = 'a F.t
  val future = F.future
  val touch = F.touch
(*
  val fork = fn fg => (print "fork!\n"; fork fg)
  val future = fn g => (print "future!\n"; future g)
  val touch = fn f => (print "touch!\n"; touch f)
*)
end

structure TreeSortArgSeq = TreeSortArg (Seq) : SORTARG
structure TreeSortArgParNoDelay = TreeSortArg (ParNoDelay) : SORTARG
structure TreeSortArgParMaybeDelay = TreeSortArg (ParMaybeDelay) : SORTARG
