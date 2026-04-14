structure CLA = CommandLineArgs

fun usage () =
  let
    val msg =
      "usage: msort-strings FILE [-grain ...] [--long] \n"
  in
    TextIO.output (TextIO.stdErr, msg);
    OS.Process.exit OS.Process.failure
  end

val filename =
  case CLA.positional () of
    [x] => x
  | _ => usage ()

val makeLong = CLA.parseFlag "long"
val quicksortGrain = CLA.parseInt "quicksort" 1024
val grain = CLA.parseInt "grain" 1024

val (contents, tm) = Util.getTime (fn _ => ReadFile.contentsSeq filename)
val _ = print ("read file in " ^ Time.fmt 4 tm ^ "s\n")
val (tokens, tm) = Util.getTime (fn _ => Tokenize.tokens Char.isSpace contents)
val _ = print ("tokenized in " ^ Time.fmt 4 tm ^ "s\n")

val prefix = CharVector.tabulate (32, fn _ => #"a")

val tokens =
  if not makeLong then tokens
  else Seq.map (fn str => prefix ^ str) tokens

val tokens = PureSeq.fromSeq tokens

fun sort cmp xs =
  if PureSeq.length xs <= quicksortGrain then
    PureSeq.quicksort cmp xs
  else
    let
      val n = PureSeq.length xs
      val l = PureSeq.take xs (n div 2)
      val r = PureSeq.drop xs (n div 2)
      val (l', r') =
        if n <= grain then
          (sort cmp l, sort cmp r)
        else
          ForkJoin.par (fn _ => sort cmp l, fn _ => sort cmp r)
    in
      PureSeq.merge cmp (l', r')
    end

val result =
  Benchmark.run "running mergesort" (fn _ => sort String.compare tokens)

(* val _ = print ("result " ^ Util.summarizeArraySlice 8 (fn x => x) result ^ "\n") *)

