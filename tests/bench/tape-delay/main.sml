structure CLA = CommandLineArgs

val infile =
  case CLA.positional () of
    [x] => x
  | _ => Util.die ("[ERR] usage: tape-delay INPUT_FILE [-output OUTPUT_FILE]\n")

val outfile = CLA.parseString "output" ""

val delayTime = CLA.parseReal "delay" 0.5
val decayFactor = CLA.parseReal "decay" 0.2

val decaydB = Real.round (20.0 * Math.log10 decayFactor)

val _ = print ("delay " ^ Real.toString delayTime ^ "s\n")
val _ = print ("decay " ^ Real.toString decayFactor ^ " ("
               ^ Int.toString decaydB ^ "dB)\n")

val (snd, tm) = Util.getTime (fn _ => NewWaveIO.readSound infile)
val _ = print ("read sound in " ^ Time.fmt 4 tm ^ "s\n")

val esnd =
  Benchmark.run "echoing" (fn _ => Signal.delay delayTime decayFactor snd)

val _ =
  if outfile = "" then
    print ("use -output file.wav to hear results\n")
  else
    let
      val (_, tm) = Util.getTime (fn _ => NewWaveIO.writeSound esnd outfile)
    in
      print ("wrote output in " ^ Time.fmt 4 tm ^ "s\n")
    end

