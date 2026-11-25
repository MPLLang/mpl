structure CLA = CommandLineArgs

val max_depth = CLA.parseInt "max_depth" 10
val num_domains = Concurrency.numberOfProcessors

val _ = print ("max_depth " ^ Int.toString max_depth ^ "\n")

datatype tree = Empty | Node of tree * tree

fun make d =
  if d = 0 then Node (Empty, Empty)
  else let val d = d-1
       in Node (make d, make d)
       end

fun check t =
  case t of
    Empty => 0
  | Node (l, r) => 1 + check l + check r

val min_depth = 4
val max_depth = Int.max (min_depth + 2, max_depth)
val stretch_depth = max_depth + 1

val _ = check (make stretch_depth)

val long_lived_tree = make max_depth

val values =
  Array.array (num_domains, 0)

fun calculate d st en ind =
  let
    val c = ref 0
  in
    Util.for (st, en+1) (fn _ =>
      c := !c + check (make d)
    );
    Array.update (values, ind, !c)
  end

fun calculate d st en ind =
  let
    val c =
      Util.loop (st, en+1) 0 (fn (c, _) =>
        c + check (make d)
      )
  in
    Array.update (values, ind, c)
  end

fun parfor g (i, j) f =
  if j-i <= 1 then
    (** MPL relies on `par` for its GC policy. This particular benchmark
      * happens to not call par on 1 processor, so let's fix that.
      *)
    (ForkJoin.par (fn _ => ForkJoin.parfor g (i, j) f, fn () => ()); ())
  else
    ForkJoin.parfor g (i, j) f


fun loop_depths d =
  Util.for (0, (max_depth - d) div 2 + 1) (fn i =>
    let
      val d = d + i * 2
      val niter = Util.pow2 (max_depth - d + min_depth)
    in
      (* ocaml source does N-way async/await loop, but this is just
       * a parallel for. *)
      parfor 1 (0, num_domains) (fn i =>
        calculate d
          (i * niter div num_domains)
          (((i + 1) * niter div num_domains) - 1)
          i);

      Array.foldl op+ 0 values;

      ()
    end)

val result = Benchmark.run "running binary trees" (fn _ =>
  let
  in
    loop_depths min_depth;
    check long_lived_tree
  end)

val _ = print ("result " ^ Int.toString result ^ "\n")
val _ = print ("values " ^ Util.summarizeArray 10 Int.toString values ^ "\n")

