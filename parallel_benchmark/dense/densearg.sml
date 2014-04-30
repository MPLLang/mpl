structure DenseArgSeq : DENSEARG = 
struct

  structure A = Array
  structure AS = ArraySlice

  type 'a t = 'a AS.slice

  val length = AS.length
  fun tabulate _ f _ n = AS.full (A.tabulate (n, f))
  val sub = AS.sub
  fun foldArray _ m i u a = 
      AS.foldl (fn (x, b) => m (i x, b)) u a
  fun foldInt _ m s u n =
    let
      fun loop (i, b) =
          if i = n then b
          else loop (i + 1, m (s i, b))
    in
      loop (0, u)
    end
  val slice = AS.subslice
  fun inParallel (f, g) = (f (), g ())
  fun GC () = if !MainUtils.explicitGC then MLton.GC.collect () else ()

end

structure DenseArgPar : DENSEARG = 
struct

  structure AS = ArraySlice

  type 'a t = 'a AS.slice

  val length = AS.length
  fun tabulate maxSeq f _ n = AS.full (MLton.Parallel.Array.tabulate maxSeq f n)
  val sub = AS.sub
  fun foldArray maxSeq m i u a = MLton.Parallel.ForkJoin.reduce maxSeq m (i o (fn j => AS.sub (a, j))) u (length a)
  val foldInt = MLton.Parallel.ForkJoin.reduce
  val slice = AS.subslice
  val inParallel = MLton.Parallel.ForkJoin.fork
  fun GC () = if !MainUtils.explicitGC then MLton.GC.collect () else ()

end

