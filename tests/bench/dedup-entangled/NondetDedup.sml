(* Phase-concurrent hash table based deduplication.
 * See https://people.csail.mit.edu/jshun/hash.pdf.
 *
 * This entangled benchmark deduplicates a sequence of 64-bit integers using a
 * phase-concurrent hash table implementation. The basic idea is that the hash
 * table stores int options, which are heap-allocated by the thread inserting
 * into the hash table. Hence, when a thread probes the table during an
 * insertion, it may CAS and load an allocation made by a concurrent thread,
 * thereby tripping the entanglement checker.
 *)

structure A = Array
structure AS = ArraySlice
val update = Array.update
val sub = Array.sub

structure Hashtbl = struct
  type 'a t = 'a option array * ('a -> int) * (('a * 'a) -> order)

  val gran = 10000

  fun create hash cmp n =
    let
      val t = ForkJoin.alloc n
      val () = ForkJoin.parfor gran (0, n) (fn i => update (t, i, NONE))
    in
      (t, hash, cmp)
    end

  fun insert (t, hash, cmp) x =
    let
      val n = A.length t
      fun nextIndex i =
        if i = n - 1 then 0
        else i + 1
      fun hash' x =
        let
          val y = hash x
        in
          if y < 0 then ~y mod n
          else y mod n
        end
      fun cmp' (x, y) =
        case (x, y) of
          (NONE, NONE) => EQUAL
        | (NONE, SOME _) => LESS
        | (SOME _, NONE) => GREATER
        | (SOME x', SOME y') => cmp (x', y')
      fun probe (i, x) =
        if not (Option.isSome x) then () else
        let
          val y = sub (t, i)
        in
          case cmp' (x, y) of
            EQUAL => ()
          | LESS => probe (nextIndex i, x)
          | GREATER =>
              let
                val z = Concurrency.casArray (t, i) (y, x)
              in
                if MLton.eq (y, z) then probe (nextIndex i, y)
                else probe (i, x)
              end
        end
    in
      probe (hash' x, SOME x)
    end

  fun keys (t, _, _) =
    let
      val n = A.length t
      val t' = SeqBasis.tabFilter gran (0, n) (fn i => sub (t, i))
    in
      AS.full t'
    end
end

(* val dedup : ('k -> int)          hash function
            -> (('k, 'k) -> order)  comparison function
            -> 'k seq               input (with duplicates)
            -> 'k seq               deduplicated (not sorted!)
*)
fun dedup hash cmp keys =
  if AS.length keys = 0 then Seq.empty () else
  let
    val n = AS.length keys
    val tbl = Hashtbl.create hash cmp (4 * n)
    val () =
      ForkJoin.parfor 100 (0, n) (fn i => Hashtbl.insert tbl (Seq.nth keys i))
  in
    Hashtbl.keys tbl
  end

(* ==========================================================================
 * now the main bit *)

structure CLA = CommandLineArgs

fun usage () =
  let
    val msg =
      "usage: dedup [--verbose] [--no-output] [-N]\n"
  in
    TextIO.output (TextIO.stdErr, msg);
    OS.Process.exit OS.Process.failure
  end

val n = CLA.parseInt "N" (100 * 1000 * 1000)

val beVerbose = CommandLineArgs.parseFlag "verbose"
val noOutput = CommandLineArgs.parseFlag "no-output"
val rep = case (Int.fromString (CLA.parseString "repeat" "1")) of
               SOME(a) => a
             | NONE => 1

fun vprint str =
  if not beVerbose then ()
  else TextIO.output (TextIO.stdErr, str)

val input = Seq.tabulate Util.hash n

fun dedupEx () =
  dedup Util.hash Int.compare input

val result = Benchmark.run "running dedup" dedupEx

fun put c = TextIO.output1 (TextIO.stdOut, c)
val _ =
  if noOutput then ()
  else
    let
      val (_, tm) = Util.getTime (fn _ =>
        ArraySlice.app (fn x => (print (Int.toString x); put #"\n")) result)
    in
      vprint ("output in " ^ Time.fmt 4 tm ^ "s\n")
    end

val _ =
  if not beVerbose then ()
  else GCStats.report ()

