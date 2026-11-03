structure CLA = CommandLineArgs
structure Gen = CityGen

(*
functor S (Sky : SKYLINE where type skyline = (int * int) Seq.t) =
struct
  open Sky
  fun skyline bs =
    case Seq.splitMid bs of
      Seq.EMPTY => Seq.empty ()
    | Seq.ONE b => singleton b
    | Seq.PAIR (l, r) =>
        let
          fun sl _ = skyline l
          fun sr _ = skyline r
          val (l', r') =
            if Seq.length bs <= 1000
            then (sl (), sr ())
            else Primitives.par (sl, sr)
        in
          combine (l', r')
        end
end

structure Stu = S (MkSkyline (structure Seq = Seq))
structure Ref = S (MkRefSkyline (structure Seq = Seq))
*)

fun pairEq ((x1, y1), (x2, y2)) = (x1 = x2 andalso y1 = y2)

fun skylinesEq (s1, s2) =
  Seq.length s1 = Seq.length s2 andalso
  Seq.reduce (fn (a,b) => a andalso b) true
  (Seq.tabulate (fn i => pairEq (Seq.nth s1 i, Seq.nth s2 i)) (Seq.length s1))

val size = CLA.parseInt "size" 1000000
val seed = CLA.parseInt "seed" 15210
val grain = CLA.parseInt "grain" 1000
val output = CLA.parseString "output" ""

(* ensure newline at end of string *)
fun println s =
  let
    val needsNewline =
      String.size s = 0 orelse String.sub (s, String.size s - 1) <> #"\n"
  in
    print (if needsNewline then s ^ "\n" else s)
  end

val _ = println ("size " ^ Int.toString size)
val _ = println ("seed " ^ Int.toString seed)
val _ = println ("grain " ^ Int.toString grain)

val (input, tm) = Util.getTime (fn _ => Gen.city size seed)
val _ = println ("generated input in " ^ Time.fmt 4 tm ^ "s\n")

val sky = Benchmark.run "skyline" (fn _ => Skyline.skyline grain input)
val _ = print ("result-len " ^ Int.toString (Seq.length sky) ^ "\n")

val _ =
  if output = "" then
    print ("use -output XXX.ppm to see result\n")
  else
  let
    val (xMin, _) = Seq.nth sky 0
    val (xMax, _) = Seq.nth sky (Seq.length sky - 1)
    val yMax = Seq.reduce Int.max 0 (Seq.map (fn (_,y) => y) sky)
    val _ = print ("xMin " ^ Int.toString xMin ^ "\n")
    val _ = print ("xMax " ^ Int.toString xMax ^ "\n")
    val _ = print ("yMax " ^ Int.toString yMax ^ "\n")

    val width = 1000
    val height = 250

    val padding = 20

    fun col x =
      padding + width * (x - xMin) div (1 + xMax - xMin)
    fun row y =
      padding + height - 1 - (height * y div (1 + yMax))

    val width' = 2*padding + width
    val height' = padding + height
    val image = Seq.tabulate (fn _ => Color.white) (width' * height')

    val _ = Seq.foreach sky (fn (idx, (x, y)) =>
      if idx >= Seq.length sky - 1 then () else
      let
        val (x', _) = Seq.nth sky (idx+1)

        val ihi = row y
        val jlo = col x
        val jhi = Int.max (col x + 1, col x')
      in
        Util.for (ihi, height') (fn i =>
          Util.for (jlo, jhi) (fn j =>
            ArraySlice.update (image, i*width' + j, Color.black)))
      end)
  in
    PPM.write output {width=width', height=height', data=image};
    print ("wrote output to " ^ output ^ "\n")
  end
