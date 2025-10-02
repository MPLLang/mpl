structure CLA = CommandLineArgs
val seed = CLA.parseInt "seed" 15210
val outfile = CLA.parseString "o" ""

val filename =
  case CLA.positional () of
    [f] => f
  | _ => Util.die ("usage: shuf [-o OUTPUT_FILE] [-seed N] INPUT_FILE")

val (contents, tm) = Util.getTime (fn _ => ReadFile.contentsSeq filename)
val _ = print ("read file in " ^ Time.fmt 4 tm ^ "s\n")


fun shuf () =
  let
    val (lines, tm) = Util.getTime (fn _ =>
      Tokenize.tokens (fn c => c = #"\n") contents)
    val _ = print ("tokenized in " ^ Time.fmt 4 tm ^ "s\n")

    val indices = Seq.tabulate (fn i => i) (Seq.length lines)
    val perm = Shuffle.shuffle indices seed
  in
    Seq.tabulate (fn i => Seq.nth lines (Seq.nth perm i)) (Seq.length lines)
  end

val result = Benchmark.run "shuffle" shuf

fun dump () =
  let
    val f = TextIO.openOut outfile
  in
    Util.for (0, Seq.length result) (fn i =>
      ( TextIO.output (f, Seq.nth result i)
      ; TextIO.output1 (f, #"\n")
      ));
    TextIO.closeOut f
  end

val _ =
  if outfile = "" then
    print ("no output specified; use -o OUTPUT_FILE to see results\n")
  else
  let
    val ((), tm) = Util.getTime dump
  in
    print ("wrote to " ^ outfile ^ " in " ^ Time.fmt 4 tm ^ "s\n")
  end
