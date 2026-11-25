structure CLA = CommandLineArgs

fun usage () =
  let
    val msg =
      "usage: msort-strings FILE\n"
  in
    TextIO.output (TextIO.stdErr, msg);
    OS.Process.exit OS.Process.failure
  end

val filename =
  case CLA.positional () of
    [x] => x
  | _ => usage ()

val makeLong = CLA.parseFlag "long"

val (contents, tm) = Util.getTime (fn _ => ReadFile.contentsSeq filename)
val _ = print ("read file in " ^ Time.fmt 4 tm ^ "s\n")
val (tokens, tm) = Util.getTime (fn _ => Tokenize.tokens Char.isSpace contents)
val _ = print ("tokenized in " ^ Time.fmt 4 tm ^ "s\n")

val prefix = CharVector.tabulate (32, fn _ => #"a")

val tokens =
  if not makeLong then tokens
  else Seq.map (fn str => prefix ^ str) tokens

val result =
  Benchmark.run "running mergesort" (fn _ => Mergesort.sort String.compare tokens)

val _ = print ("result " ^ Util.summarizeArraySlice 8 (fn x => x) result ^ "\n")

