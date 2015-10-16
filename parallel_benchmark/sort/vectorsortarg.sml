structure VectorSliceSortArgSeq : SORTARG =
struct

  structure V = Vector
  open VectorSlice

  type t = real slice

  exception Sort

  fun fork (f, g) = (f (), g ())
  type 'a future = 'a
  fun future f = f ()
  fun touch a = a

  fun fromList _ = full o V.fromList
  fun toList a = foldr (fn (x, l) => x :: l) nil a
  val empty = full (V.fromList (nil : real list))
  fun singleton x = full (V.tabulate (1, fn _ => x))
  (* PERF ? *)
  fun append (a, b) = full (concat [a, b])
  val concat = full o concat
  (* PERF *)
  fun filter _ f = full o V.fromList o (List.filter f) o toList

  fun pivot a = sub (a, Random.randomInt (length a))

  fun update _ = raise Sort
  fun copy _ = raise Sort
  local
    structure V = MLton.Vector
  in
  fun unfoldi _ = fn args =>
                     let val (a, x) = V.unfoldi args
                     in
                       (full a, x)
                     end
  end
  val slice = subslice

  fun halve a =
      let
        val l = length a div 2
      in
        (subslice (a, 0, SOME l), subslice (a, l, NONE))
      end

  fun printArg a = print (foldr (fn (x, s) => Real.toString x
                                              ^ ", " ^ s) "\n" a)

  fun GC () = if !MainUtils.explicitGC then MLton.GC.collect () else ()

end

structure VectorSliceSortArgParNoDelay : SORTARG =
struct
  open VectorSliceSortArgSeq

  (* XXX make filter respect maxSeq *)

  val fork = MLton.Parallel.ForkJoin.fork
  (* type 'a future = 'a MLton.Parallel.FutureSuspend.t *)
  (* val future = MLton.Parallel.FutureSuspend.future *)
  (* val touch = MLton.Parallel.FutureSuspend.touch *)

end

structure VectorSliceSortArgParMaybeDelay : SORTARG =
struct
  open VectorSliceSortArgSeq

  (* XXX make filter respect maxSeq *)

  val fork = MLton.Parallel.ForkJoin.fork
  (* type 'a future = 'a MLton.Parallel.FutureSuspendMaybeDelay.t *)
  (* val future = MLton.Parallel.FutureSuspendMaybeDelay.future *)
  (* val touch = MLton.Parallel.FutureSuspendMaybeDelay.touch *)

end

structure ArraySliceSortArgSeq : SORTARG =
struct

  structure A = Array
  structure AS = ArraySlice
  open ArraySlice
  type t = real slice

  exception Sort

  fun fork (f, g) = (f (), g ())
  type 'a future = 'a
  fun future f = f ()
  fun touch a = a

  fun fromList _ = full o A.fromList
  fun toList a = foldr (fn (x, l) => x :: l) nil a
  val empty = full (A.fromList (nil : real list))
  fun singleton x = full (A.tabulate (1, fn _ => x))
  fun concat _ = raise Sort
  fun append _ = raise Sort
  fun unfoldi _ _ = raise Sort

  (* PERF *)
  fun filter _ f = full o A.fromList o (List.filter f) o toList

  fun pivot a = sub (a, Random.randomInt (length a))

  fun copy { src, dst, di } =
      let
        val (a, i, _) = base dst
      in
        AS.copy { src = src, dst = a, di = di + i }
      end

  val slice = subslice

  fun halve a =
      let
        val l = length a div 2
      in
        (subslice (a, 0, SOME l), subslice (a, l, NONE))
      end

  fun printArg a = print (foldr (fn (x, s) => Real.toString x
                                              ^ ", " ^ s) "\n" a)

  fun GC () = if !MainUtils.explicitGC then MLton.GC.collect () else ()

end

structure ArraySliceSortArgPar : SORTARG =
struct
  open ArraySliceSortArgSeq

  (* XXX make filter respect maxSeq *)

  val fork = MLton.Parallel.ForkJoin.fork
  (* type 'a future = 'a MLton.Parallel.FutureSuspend.t *)
  (* val future = MLton.Parallel.FutureSuspend.future *)
  (* val touch = MLton.Parallel.FutureSuspend.touch *)

end
