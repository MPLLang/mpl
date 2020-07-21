fun usage () =
  let
    val msg =
      "usage: tokens [--benchmark] FILE\n"
  in
    TextIO.output (TextIO.stdErr, msg);
    OS.Process.exit OS.Process.failure
  end

val filename =
  case CommandLineArgs.positional () of
    [x] => x
  | _ => usage ()

val doBenchmark = CommandLineArgs.parseFlag "benchmark"

fun bprint str =
  if not doBenchmark then ()
  else print (str ^ "\n")

val (contents, tm) = Util.getTime (fn _ => ReadFile.contentsSeq filename)
val _ = bprint ("read file in " ^ Time.fmt 4 tm ^ "s")

val (tokens, tm) = Util.getTime (fn _ => Tokenize.tokensSeq Char.isSpace contents)
val _ = bprint ("tokenized in " ^ Time.fmt 4 tm ^ "s")

fun put c = TextIO.output1 (TextIO.stdOut, c)
fun putToken token =
  Util.for (0, Seq.length token) (put o Seq.nth token)

val _ =
  if doBenchmark then
    let
    in
      bprint ("number of tokens: " ^ Int.toString (Seq.length tokens))
    end
  else
    let
      val (_, tm) = Util.getTime (fn _ =>
        ArraySlice.app (fn token => (putToken token; put #"\n")) tokens)
    in
      (* bprint ("output in " ^ Time.fmt 4 tm ^ "s\n") *)
      ()
    end
