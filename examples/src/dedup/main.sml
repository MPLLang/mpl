structure CLA = CommandLineArgs

fun usage () =
  let
    val msg =
      "usage: dedup [--benchmark] FILE\n"
  in
    TextIO.output (TextIO.stdErr, msg);
    OS.Process.exit OS.Process.failure
  end

val filename =
  case CLA.positional () of
    [x] => x
  | _ => usage ()

val doBenchmark = CommandLineArgs.parseFlag "benchmark"

fun bprint str =
  if not doBenchmark then ()
  else print (str ^ "\n")

fun toWord str =
  let
    (* just cap at 32 for long strings *)
    val n = Int.min (32, String.size str)
    fun c i = Word64.fromInt (Char.ord (String.sub (str, i)))
    fun loop h i =
      if i >= n then h
      else loop (Word64.+ (Word64.* (h, 0w31), c i)) (i+1)
  in
    loop 0w7 0
  end

fun hash1 str = Util.hash64 (toWord str)
fun hash2 str = Util.hash64 (toWord str + 0w1111111)

val (contents, tm) = Util.getTime (fn _ => ReadFile.contentsSeq filename)
val _ = bprint ("read file in " ^ Time.fmt 4 tm ^ "s")

val (tokens, tm) = Util.getTime (fn _ => Tokenize.tokens Char.isSpace contents)
val _ = bprint ("tokenized in " ^ Time.fmt 4 tm ^ "s")

val (result, tm) = Util.getTime (fn _ => Dedup.dedup op= hash1 hash2 tokens)
val _ = bprint ("deduplicated in " ^ Time.fmt 4 tm ^ "s")

fun put c = TextIO.output1 (TextIO.stdOut, c)
val _ =
  if doBenchmark then
    let
    in
      bprint ("number of tokens: " ^ Int.toString (Seq.length tokens));
      bprint ("number of unique tokens: " ^ Int.toString (Seq.length result))
    end
  else
    let
      val (_, tm) = Util.getTime (fn _ =>
        ArraySlice.app (fn token => (print token; put #"\n")) result)
    in
      (* bprint ("output in " ^ Time.fmt 4 tm ^ "s") *)
      ()
    end
