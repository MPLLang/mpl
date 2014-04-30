
structure ListSortArgSeq : SORTARG =
struct

  type t = real list

  exception Sort

  fun fork (f, g) = (f (), g ())
  type 'a future = 'a
  fun future f = f ()
  fun touch a = a

  val length = length
  val sub = List.nth

  fun fromList _ a = a
  fun toList a = a

  fun foldli f y a = 
    let 
      fun loop nil i y = y
        | loop (x::xs) i y = loop xs (i + 1) (f (i, x, y))
    in
      loop a 0 y
    end

  val empty = nil
  fun singleton x = [x]
  val concat = List.concat
  fun append (a, b) = a @ b

  fun update _ = raise Sort
  fun copy _ = raise Sort

  fun unfoldi _ (n, y : 'a, f) = 
      let
        fun loop i y acc = 
            if i = n then (acc, y)
            else 
              let 
                val (x, y) = f (i, y)
              in
                loop (i + 1) y (x::acc)
              end

        val (a, y) = loop 0 y nil
      in
        (rev a, y)
      end

  fun filter _ = List.filter

  fun pivot nil = raise Sort
    | pivot (x::xs) = x

  fun copy _ = raise Sort

  fun slice (a, i, NONE) = List.drop (a, i)
    | slice (a, i, SOME j) = List.take (List.drop (a, i), j)

  fun halve a = 
      let 
        val l = length a div 2
      in
        (List.take (a, l), List.drop (a, l))
      end

  fun printArg a = print (foldr (fn (x, s) => Real.toString x
                                              ^ ", " ^ s) "\n" a)

  fun GC () = if !MainUtils.explicitGC then MLton.GC.collect () else ()

end

structure ListSortArgParNoDelay : SORTARG =
struct
  open ListSortArgSeq

  fun filter maxSeq f l = 
      let 
        fun loop 0 l acc = (MLton.Parallel.Basic.yield (); loop maxSeq l acc)
          | loop i nil acc = acc
          | loop i (x::xs) acc = if f x then loop (i - 1) xs (x::acc)
                                 else loop (i - 1) xs acc
      in
        rev (loop maxSeq l nil)
      end

  val fork = MLton.Parallel.ForkJoin.fork
  type 'a future = 'a MLton.Parallel.FutureSuspend.t
  val future = MLton.Parallel.FutureSuspend.future
  val touch = MLton.Parallel.FutureSuspend.touch

end

structure ListSortArgParMaybeDelay : SORTARG =
struct
  open ListSortArgSeq

  fun filter maxSeq f l = 
      let 
        fun loop 0 l acc = (MLton.Parallel.Basic.yield (); loop maxSeq l acc)
          | loop i nil acc = acc
          | loop i (x::xs) acc = if f x then loop (i - 1) xs (x::acc)
                                 else loop (i - 1) xs acc
      in
        rev (loop maxSeq l nil)
      end

  val fork = MLton.Parallel.ForkJoin.fork
  type 'a future = 'a MLton.Parallel.FutureSuspendMaybeDelay.t
  val future = MLton.Parallel.FutureSuspendMaybeDelay.future
  val touch = MLton.Parallel.FutureSuspendMaybeDelay.touch
(*
  fun halve a = 
      let 
        val l = length a div 2
      in
        fork (fn () => List.take (a, l), 
              fn () => List.drop (a, l)))
      end
*)
end
