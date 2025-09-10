structure CLA = CommandLineArgs
structure Seq = ArraySequence

val n = CLA.parseInt "n" (1000 * 1000 * 100)
val filePath = CLA.parseString "infile" ""

val source =
  if filePath = "" then
    (*Seq.tabulate (fn _ => #" ") n*)
    Seq.tabulate (fn i => Char.chr (Util.hash i mod 255)) n
  else
    let
      val (source, tm) = Util.getTime (fn _ => ReadFile.contentsSeq filePath)
      val _ = print ("loadtime " ^ Time.fmt 3 tm ^ "\n")
    in
      source
    end

fun task () =
  WC.wc source

fun check (lines, words, bytes) =
  let
  in
    print ("correct? checker for wc not implemented yet\n")
  end

val (nl, nw, nb) = Benchmark.run "wc" task
val _ = print ("lines " ^ Int.toString nl ^ "\n")
val _ = print ("words " ^ Int.toString nw ^ "\n")
val _ = print ("chars " ^ Int.toString nb ^ "\n")
