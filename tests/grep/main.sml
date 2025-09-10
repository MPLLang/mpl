structure CLA = CommandLineArgs
structure Seq = ArraySequence

(* chosen by subdirectory *)
structure Grep = MkGrep(OldDelayedSeq)

(*
val pattern = CLA.parseString "pattern" ""
val filePath = CLA.parseString "infile" ""

val input =
  let
    val (source, tm) = Util.getTime (fn _ => ReadFile.contentsSeq filePath)
    val _ = print ("loadtime " ^ Time.fmt 4 tm ^ "s\n")
  in
    source
  end

val pattern =
  Seq.tabulate (fn i => String.sub (pattern, i)) (String.size pattern)
*)

val (pat, file) =
  case CLA.positional () of
    [pat, file] => (pat, file)
  | _ => Util.die ("[ERR] usage: grep PATTERN FILE")

val (input, tm) = Util.getTime (fn _ => ReadFile.contentsSeq file)
val _ = print ("read file in " ^ Time.fmt 4 tm ^ "s\n")

val pattern = Seq.tabulate (fn i => String.sub (pat, i)) (String.size pat)

val n = Seq.length input
val _ = print ("n " ^ Int.toString n ^ "\n")

fun task () =
  Grep.grep pattern input

val result = Benchmark.run "running grep" task
val _ = print ("num matching lines " ^ Int.toString (Seq.length result) ^ "\n")

(* fun dumpLoop i =
  if i >= Seq.length result then () else
  let
    val (s, e) = Seq.nth result i
    val tok = CharVector.tabulate (e-s, fn k => Seq.nth input (s+k))
  in
    print tok;
    print "\n";
    dumpLoop (i+1)
  end

val _ = dumpLoop 0 *)
