structure CLA = CommandLineArgs

local
open GIF
in

fun encode palette {width, height, numImages, getImage} =
  if numImages <= 0 then
    err "Must be at least one image"
  else
  let
    val width16 = checkToWord16 "width" width
    val height16 = checkToWord16 "height" height

    val numberOfColors = Seq.length (#colors palette)

    val _ =
      if numberOfColors <= 256 then ()
      else err "Must have at most 256 colors in the palette"

    val imageData =
      AS.full (SeqBasis.tabulate 1 (0, numImages) (fn i =>
        let
          val img = getImage i
        in
          if Seq.length img <> height * width then
            err "Not all images are the right dimensions"
          else
            LZW.packCodeStream numberOfColors
              (LZW.codeStream numberOfColors img)
        end))
  in
    imageData
  end

end

val width = CLA.parseInt "width"
val height = CLA.parseInt "height"

fun pixel (i, j) =
  Color.hsv
    { h = 90.0 + (Real.fromInt i / Real.fromInt width) * 135.0
    , s = 0.5 + (Real.fromInt j / Real.fromInt height) * 0.5
    , v = 0.8
    }

val image =
  { height = height
  , width = width
  , data = Seq.tabulate (fn i => (i div width, i mod width)) (width * height)
  }

val imageData =

