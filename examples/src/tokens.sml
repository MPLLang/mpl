fun usage () =
  let
    val msg =
      "usage: tokens [--verbose] [--no-output] FILE\n"
  in
    TextIO.output (TextIO.stdErr, msg);
    OS.Process.exit OS.Process.failure
  end

val filename =
  case CommandLineArgs.positional () of
    [x] => x
  | _ => usage ()

val beVerbose = CommandLineArgs.parseFlag "verbose"
val noOutput = CommandLineArgs.parseFlag "no-output"

fun vprint str =
  if not beVerbose then ()
  else TextIO.output (TextIO.stdErr, str)

val (contents, tm) = Util.getTime (fn _ => ReadFile.contentsSeq filename)
val _ = vprint ("read file in " ^ Time.fmt 4 tm ^ "s\n")

val (tokens, tm) = Util.getTime (fn _ => Tokenize.tokensSeq Char.isSpace contents)
val _ = vprint ("tokenized in " ^ Time.fmt 4 tm ^ "s\n")

fun put c = TextIO.output1 (TextIO.stdOut, c)
fun putToken token =
  Util.for (0, Seq.length token) (put o Seq.nth token)

val _ =
  if noOutput then ()
  else
    let
      val (_, tm) = Util.getTime (fn _ =>
        ArraySlice.app (fn token => (putToken token; put #"\n")) tokens)
    in
      vprint ("output in " ^ Time.fmt 4 tm ^ "s\n")
    end
