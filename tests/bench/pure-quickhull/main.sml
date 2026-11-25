structure CLA = CommandLineArgs

val resolution = 1000000
fun randReal seed =
  Real.fromInt (Util.hash seed mod resolution) / Real.fromInt resolution

fun randPt seed =
  let
    val r = Math.sqrt (randReal (2*seed))
    val theta = randReal (2*seed+1) * 2.0 * Math.pi
  in
    (1.0 + r * Math.cos(theta), 1.0 + r * Math.sin(theta))
  end

(* val filename = CLA.parseString "infile" "" *)
val outfile = CLA.parseString "outfile" ""
val n = CLA.parseInt "N" (1000 * 1000 * 100)

val _ = print ("input size " ^ Int.toString n ^ "\n")

val (inputPts, tm) = Util.getTime (fn _ => PureSeq.tabulate randPt n)
val _ = print ("generated input in " ^ Time.fmt 4 tm ^ "s\n")

val result = Benchmark.run "running quickhull" (fn _ => Quickhull.hull inputPts)

val _ = print ("hull size " ^ Int.toString (PureSeq.length result) ^ "\n")

fun rtos x =
  if x < 0.0 then "-" ^ rtos (~x)
  else Real.fmt (StringCvt.FIX (SOME 3)) x
fun pttos (x,y) =
  String.concat ["(", rtos x, ",", rtos y, ")"]

(* fun check result =
  let
    val correct = Checkhull.check inputPts result
  in
    print ("correct? " ^
      Checkhull.report inputPts result (Checkhull.check inputPts result)
      ^ "\n")
  end *)

val _ =
  if outfile = "" then
    print ("use -outfile XXX to see result\n")
  else
    let
      val out = TextIO.openOut outfile
      fun writeln str = TextIO.output (out, str ^ "\n")
      fun dump i =
        if i >= PureSeq.length result then ()
        else (writeln (Int.toString (PureSeq.nth result i)); dump (i+1))
    in
      writeln "pbbs_sequenceInt";
      dump 0;
      TextIO.closeOut out
    end

