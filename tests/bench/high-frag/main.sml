structure CLA = CommandLineArgs
val nodes = CLA.parseInt "nodes-per-tree" 1000000
val numTrees = CLA.parseInt "num-trees" 100
val grain = CLA.parseInt "grain" 1
val _ = print ("nodes-per-tree " ^ Int.toString nodes ^ "\n")
val _ = print ("num-trees " ^ Int.toString numTrees ^ "\n")

datatype tree = Leaf of int | Node of tree * tree


fun go (i, j) =
  case j-i of
    1 => Leaf (Util.hash i)
  | n =>
      if n <= grain then
        Node (go (i, i + n div 2), go (i + n div 2, j))
      else
        Node (ForkJoin.par (fn _ => go (i, i + n div 2),
                            fn _ => go (i + n div 2, j)))

fun benchmark () =
  List.tabulate (numTrees, fn i => go (i*nodes, (i+1)*nodes))

val results =
  Benchmark.run "high-fragmentation tree" benchmark


(** ======================================================================
  * Now do some arbitrary computation on the result to make sure it's not
  * optimized out.
  *)

fun reduce f t =
  let
    fun loop depth t =
      case t of
        Leaf x => x
      | Node (a, b) =>
          if depth > 10 then
            f (loop (depth+1) a, loop (depth+1) b)
          else
            f (ForkJoin.par (fn _ => loop (depth+1) a,
                             fn _ => loop (depth+1) b))
  in
    loop 0 t
  end

val foo =
  List.foldl Int.min (valOf Int.maxInt) (List.map (reduce Int.max) results)

val _ = print ("foo " ^ Int.toString foo ^ "\n")
