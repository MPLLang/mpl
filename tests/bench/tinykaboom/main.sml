structure CLA = CommandLineArgs

val fps = CLA.parseInt "fps" 60
val width = CLA.parseInt "width" 640
val height = CLA.parseInt "height" 480
val frames = CLA.parseInt "frames" (10 * fps)
val outfile = CLA.parseString "outfile" ""
(* val frame = CLA.parseInt "frame" 100 *)

val _ = print ("width " ^ Int.toString width ^ "\n")
val _ = print ("height " ^ Int.toString height ^ "\n")
(* val _ = print ("frame " ^ Int.toString frame ^ "\n") *)
val _ = print ("fps " ^ Int.toString fps ^ "\n")
val _ = print ("frames " ^ Int.toString frames ^ "\n")

val duration = Real.fromInt frames / Real.fromInt fps

val _ = print ("(" ^ Real.fmt (StringCvt.FIX (SOME 2)) duration ^ " seconds)\n")

fun bench () =
  let
    val _ = print ("generating frames...\n")
    val (images, tm) = Util.getTime (fn _ =>
      SeqBasis.tabulate 1 (0, frames) (fn frame =>
        { width = width
        , height = height
        , data =
            ArraySlice.full
            (TinyKaboom.frame (f32.fromInt frame / f32.fromInt fps) width height)
        }))
    val _ = print ("generated all frames in " ^ Time.fmt 4 tm ^ "s\n")
    val perFrame = Time.fromReal (Time.toReal tm / Real.fromInt frames)
    val _ = print ("average time per frame: " ^ Time.fmt 4 perFrame ^ "s\n")
  in
    images
  end

val images = Benchmark.run "tinykaboom" bench

val _ =
  if outfile = "" then
    print ("no output file specified; use -outfile XXX.gif to see result\n")
  else
  let
    val _ = print ("generating palette...\n")
    (* val palette = GIF.Palette.summarize [Color.white, Color.black] 256
      { width = width
      , height = height
      , data = ArraySlice.full (TinyKaboom.frame 5.1667 640 480)
      } *)

    fun sampleColor i =
      let
        val k = Util.hash i
        val frame = (k div (width*height)) mod frames
        val idx = k mod (width*height)
      in
        Seq.nth (#data (Array.sub (images, frame))) idx
      end

    val palette = GIF.Palette.summarizeBySampling [Color.white, Color.black] 256
      sampleColor

    val blowUpFactor = CLA.parseInt "blowup" 1
    val _ = print ("blowup " ^ Int.toString blowUpFactor ^ "\n")

    fun blowUpImage (image as {width, height, data}) =
      if blowUpFactor = 1 then image else
      let
        val width' = blowUpFactor * width
        val height' = blowUpFactor * height
        val output = ForkJoin.alloc (width' * height')
        val _ =
          ForkJoin.parfor 1 (0, height) (fn i =>
            ForkJoin.parfor (1000 div blowUpFactor) (0, width) (fn j =>
              let
                val c = Seq.nth data (i*width + j)
              in
                Util.for (0, blowUpFactor) (fn di =>
                  Util.for (0, blowUpFactor) (fn dj =>
                    Array.update (output, (i*blowUpFactor+di)*width' + (j*blowUpFactor+dj), c)))
              end))
      in
        { width = width'
        , height = height'
        , data = ArraySlice.full output
        }
      end

    val _ = print ("writing to " ^ outfile ^"...\n")
    val msBetween = Real.round ((1.0 / Real.fromInt fps) * 100.0)
    val (_, tm) = Util.getTime (fn _ =>
      GIF.writeMany outfile msBetween palette
        { width = blowUpFactor * width
        , height = blowUpFactor * height
        , numImages = frames
        , getImage = fn i => #remap palette (blowUpImage (Array.sub (images, i)))
        })
    val _ = print ("wrote all frames in " ^ Time.fmt 4 tm ^ "s\n")
  in
    ()
  end
