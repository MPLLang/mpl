(* XX unify this with the MLton version *)
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

  fun fromList _ args = (full o V.fromList) args
  fun toList a = foldr (fn (x, l) => x :: l) nil a
  val empty = full (V.fromList (nil : real list))
  fun singleton x = full (V.tabulate (1, fn _ => x))
  (* PERF ? *)
  fun append (a, b) = full (concat [a, b])
  val concat = fn args => (full o concat) args
  (* PERF *)
  fun filter _ f = full o V.fromList o (List.filter f) o toList

  fun pivot a = sub (a, Random.randomInt (length a))

  fun update _ = raise Sort
  fun copy _ = raise Sort
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

structure VectorSliceSortArgParNoDelay : SORTARG = VectorSliceSortArgSeq
structure VectorSliceSortArgParMaybeDelay : SORTARG = VectorSliceSortArgSeq

structure ArraySliceSortArgSeq : SORTARG =
struct

  type t = real ArraySlice.slice

  exception Sort

  fun fork _ = raise Sort
  type 'a future = 'a
  fun future _ = raise Sort
  fun touch _ = raise Sort

  fun length _ = raise Sort
  fun fromList _ = raise Sort
  fun toList _ = raise Sort
  val empty = ArraySlice.full (Array.fromList (nil : real list))
  fun singleton _ = raise Sort
  fun append _ = raise Sort
  fun concat _ = raise Sort
  fun filter _ = raise Sort

  fun sub _ = raise Sort
  fun pivot _ = raise Sort

  fun update _ = raise Sort
  fun copy _ = raise Sort
  fun foldli _ = raise Sort
  fun unfoldi _ = raise Sort
  fun slice _ = raise Sort
  fun halve _ = raise Sort

  fun printArg _ = raise Sort

  fun GC _ = raise Sort

end

structure ArraySliceSortArgPar : SORTARG = ArraySliceSortArgSeq
