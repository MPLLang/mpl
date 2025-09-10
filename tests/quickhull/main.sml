structure CLA = CommandLineArgs
structure Seq = ArraySequence

structure Quickhull = MkQuickhull(OldDelayedSeq)

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

val filename = CLA.parseString "infile" ""
val outfile = CLA.parseString "outfile" ""
val n = CLA.parseInt "n" (1000 * 1000 * 100)
val doCheck = CLA.parseFlag "check"

(* This silly thing helps ensure good placement, by
 * forcing points to be reallocated more adjacent.
 * It's a no-op, but gives us as much as 2x time
 * improvement (!)
 *)
fun swap pts = Seq.map (fn (x, y) => (y, x)) pts
fun compactify pts = swap (swap pts)

val inputPts =
  case filename of
    "" => Seq.tabulate randPt n
  | _ => compactify (ParseFile.readSequencePoint2d filename)

val n = Seq.length inputPts

fun task () =
  Quickhull.hull inputPts

fun rtos x =
  if x < 0.0 then "-" ^ rtos (~x)
  else Real.fmt (StringCvt.FIX (SOME 3)) x
fun pttos (x,y) =
  String.concat ["(", rtos x, ",", rtos y, ")"]

(*
fun check result =
  if not doCheck then () else
  let
    val correct = Checkhull.check inputPts result
  in
    print ("correct? " ^
      Checkhull.report inputPts result (Checkhull.check inputPts result)
      ^ "\n")
  end
*)

(* val _ =
  (writeln "pbbs_sequencePoint2d"; dump inputPts 0; OS.Process.exit OS.Process.success) *)

val result = Benchmark.run "quickhull" task
val _ = print ("hull size " ^ Int.toString (Seq.length result) ^ "\n")
(* val _ = check result *)

val _ =
  if outfile = "" then () else
  let
    val out = TextIO.openOut outfile
    fun writeln str = TextIO.output (out, str ^ "\n")
    fun dump i =
      if i >= Seq.length result then ()
      else (writeln (Int.toString (Seq.nth result i)); dump (i+1))
  in
    writeln "pbbs_sequenceInt";
    dump 0;
    TextIO.closeOut out
  end


(* fun dumpPt (x, y) = writeln (rtos x ^ " " ^ rtos y) *)
(* fun dump pts i =
  if i >= Seq.length pts then ()
  else (dumpPt (Seq.nth pts i); dump pts (i+1)) *)
(* val hullPts = Seq.map (Seq.nth inputPts) result *)
(* dump hullPts 0 *)
