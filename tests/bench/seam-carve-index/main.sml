structure CLA = CommandLineArgs

val filename =
  case CLA.positional () of
    [x] => x
  | _ => Util.die "missing filename"

val numSeams = CLA.parseInt "num-seams" 100
val _ = print ("num-seams " ^ Int.toString numSeams ^ "\n")

val (image, tm) = Util.getTime (fn _ => PPM.read filename)
val _ = print ("read image in " ^ Time.fmt 4 tm ^ "s\n")

val w = #width image
val h = #height image

val _ = print ("height " ^ Int.toString h ^ "\n")
val _ = print ("width " ^ Int.toString w ^ "\n")

val _ =
  if numSeams >= 0 andalso numSeams <= w then ()
  else
    Util.die ("cannot remove " ^ Int.toString numSeams
    ^ " seams from image of width " ^ Int.toString w ^ "\n")

val X = Benchmark.run "seam carving"
  (fn _ => SCI.makeSeamCarveIndex numSeams image)

val outfile = CLA.parseString "output" ""

val _ =
  if outfile = "" then
    print ("use -output XXX.gif to see result\n")
  else
    let
      val ((palette, indices), tm) = Util.getTime (fn _ =>
        let
          val palette = GIF.Palette.summarize [Color.black, Color.red] 128 image
        in
          (palette, #remap palette image)
        end)

      val _ = print ("remapped color palette in " ^ Time.fmt 4 tm ^ "s\n")
      fun getIdx (i, j) = Seq.nth indices (i*w + j)

      val redIdx = GIF.Palette.remapColor palette Color.red
      val blackIdx = GIF.Palette.remapColor palette Color.black

      fun removeSeams count =
        let
          val data = ForkJoin.alloc (w * h)
          fun set (i, j) x = Array.update (data, i*w + j, x)

          (* compact row i from index j, writing the result at index k *)
          fun compactRow i j k =
            if j >= w then
              Util.for (k, w) (fn kk => set (i, kk) blackIdx)
            else
              let
                val xx = Seq.nth X (i*w + j)
              in
                if xx = ~1 orelse xx > count then
                  ( set (i, k) (getIdx (i, j))
                  ; compactRow i (j+1) (k+1)
                  )
                else if xx = count then
                  ( set (i, k) redIdx
                  ; compactRow i (j+1) (k+1)
                  )
                else
                  compactRow i (j+1) k
              end
        in
          ForkJoin.parfor 1 (0, h) (fn i => compactRow i 0 0);
          ArraySlice.full data
        end

      val (images, tm) = Util.getTime (fn _ =>
        ArraySlice.full (SeqBasis.tabulate 1 (0, numSeams+1) removeSeams))
      val _ = print ("generated images in " ^ Time.fmt 4 tm ^ "s\n")

      val (_, tm) = Util.getTime (fn _ =>
        GIF.writeMany outfile 10 palette
          { width = w
          , height = h
          , numImages = numSeams+1
          , getImage = Seq.nth images
          })
    in
      print ("wrote to " ^ outfile ^ " in " ^ Time.fmt 4 tm ^ "s\n")
    end

(* val _ =
  if outfile = "" then
    print ("use -output XXX to see result\n")
  else
    let
      fun colorSeam i =
        Color.hsv { h = 100.0 * (Real.fromInt i / Real.fromInt numSeams)
                  , s = 1.0
                  , v = 1.0
                  }

      val carved =
        { width = w
        , height = h
        , data = ArraySlice.full (SeqBasis.tabulate 4000 (0, w * h) (fn k =>
            let
              val i = k div w
              val j = k mod w
            in
              if Seq.nth X k < 0 then
                PPM.elem image (i, j)
              else
                colorSeam (Seq.nth X k)
            end))
        }
      val (_, tm) = Util.getTime (fn _ => PPM.write outfile carved)
    in
      print ("wrote output in " ^ Time.fmt 4 tm ^ "s\n")
    end *)

