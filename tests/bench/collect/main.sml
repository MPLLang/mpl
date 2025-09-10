structure CLA = CommandLineArgs
val n = CLA.parseInt "n" 10000000
val k = CLA.parseInt "k" 1000
val seed = CLA.parseInt "seed" 15210
val impl = CLA.parseString "impl" "sort"

val _ = print ("n    " ^ Int.toString n ^ "\n")
val _ = print ("k    " ^ Int.toString k ^ "\n")
val _ = print ("seed " ^ Int.toString seed ^ "\n")
val _ = print ("impl " ^ impl ^ "\n")

fun gen_real seed =
  Real.fromInt (Util.hash seed mod 100000000) / 100000000.0

fun gen_elem i =
  (Util.hash (seed + 2 * i) mod k, gen_real (seed + 2 * i + 1))

val kvs = Seq.tabulate gen_elem n

structure K =
struct
  type t = int
  fun equal (x: t, y: t) = (x = y)
  fun cmp (x, y) = Int.compare (x, y)
  val empty = ~1
  val hash = Util.hash
end

structure V = struct type t = real val zero = 0.0 val combine = Real.+ end


structure CollectSort = CollectSort (structure K = K structure V = V)
structure CollectHash = CollectHash (structure K = K structure V = V)

fun bench () =
  case impl of
    "sort" => CollectSort.collect kvs
  | "hash" => CollectHash.collect kvs
  | _ => Util.die "unknown impl"

val result = Benchmark.run "collect" bench

val _ = print ("num unique keys: " ^ Int.toString (Seq.length result) ^ "\n")
val _ = print
  ("result: "
   ^
   Util.summarizeArraySlice 10
     (fn (k, v) => "(" ^ Int.toString k ^ "," ^ Real.toString v ^ ")") result
   ^ "\n")
