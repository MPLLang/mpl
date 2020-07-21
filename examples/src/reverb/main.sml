structure CLA = CommandLineArgs

fun usage () =
  let
    val msg =
      "usage: reverb INPUT.wav [-output OUTPUT.wav]\n"
  in
    TextIO.output (TextIO.stdErr, msg);
    OS.Process.exit OS.Process.failure
  end

val infile =
  case CLA.positional () of
    [x] => x
  | _ => usage ()

val outfile = CLA.parseString "output" ""

val (snd, tm) = Util.getTime (fn _ => NewWaveIO.readSound infile)
val _ = print ("read sound in " ^ Time.fmt 4 tm ^ "s\n")

val (rsnd, tm) = Util.getTime (fn _ => Signal.reverb snd)
val _ = print ("reverberated in " ^ Time.fmt 4 tm ^ "s\n")

val _ =
  if outfile = "" then
    print ("use -output file.wav to hear results\n")
  else
    let
      val (_, tm) = Util.getTime (fn _ => NewWaveIO.writeSound rsnd outfile)
    in
      print ("wrote output in " ^ Time.fmt 4 tm ^ "s\n")
    end
