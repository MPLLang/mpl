structure  CLA = CommandLineArgs

(* ==========================================================================
 * parse command-line arguments and run
 *)

val numElems = CLA.parseInt "num-elems" (1000 * 1000 * 10)
val numSeqs = CLA.parseInt "num-seqs" (numElems div 5)
val impl = CLA.parseString "impl" "lib"
val _ = print ("num-elems " ^ Int.toString numElems ^ "\n")
val _ = print ("num-seqs  " ^ Int.toString numSeqs ^ "\n")

val doit =
  case impl of
    "lib" => Seq.flatten
  | "full-expand-pow2" => FullExpandPow2Flatten.flatten
  | "all-bs" => AllBSFlatten.flatten
  | "blocked-all-bs" => BlockedAllBSFlatten.flatten
  | "multi-blocked-bs" => MultiBlockedBSFlatten.flatten
  | "expand" => ExpandFlatten.flatten
  | "simple-blocked" => SimpleBlockedFlatten.flatten
  | "simple-expand" => SimpleExpandFlatten.flatten
  | _ => raise Fail ("unknown impl '" ^ impl ^ "'")

val _ = print ("impl      " ^ impl ^ "\n")

val offsets =
  Mergesort.sort Int.compare
  (Seq.tabulate (fn i => Util.hash i mod numElems) numSeqs)
fun O i =
  if i = 0 then 0
  else if i >= numSeqs then numElems
  else Seq.nth offsets i
val elems = Seq.tabulate (fn i => i) numElems
val input = Seq.tabulate (fn i => Seq.subseq elems (O i, O (i+1) - O i)) numSeqs
val _ = print ("generated input\n")

val result = Benchmark.run "flatten" (fn _ => doit input)

val correct = Seq.equal op= (elems, result)
val _ = print ("correct? " ^ (if correct then "yes" else "no") ^ "\n")
val _ = print ("result " ^ Util.summarizeArraySlice 10 Int.toString result ^ "\n")

