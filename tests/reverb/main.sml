structure CLA = CommandLineArgs



val infile =
  case CLA.positional () of
    [x] => x
  | _ => Util.die ("[ERR] usage: reverb INPUT_FILE [-output OUTPUT_FILE]\n")

val outfile = CLA.parseString "output" ""

val (snd, tm) = Util.getTime (fn _ => NewWaveIO.readSound infile)
val _ = print ("read sound in " ^ Time.fmt 4 tm ^ "s\n")

val rsnd = Benchmark.run "reverberating" (fn _ => Signal.reverb snd)

val _ =
  if outfile = "" then
    print ("use -output file.wav to hear results\n")
  else
    let
      val (_, tm) = Util.getTime (fn _ => NewWaveIO.writeSound rsnd outfile)
    in
      print ("wrote output in " ^ Time.fmt 4 tm ^ "s\n")
    end

