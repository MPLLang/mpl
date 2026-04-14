structure CLA = CommandLineArgs

val (input, output) =
  case CLA.positional () of
    [input, output] => (input, output)
  | _ => Util.die "missing filename"

val (image, tm) = Util.getTime (fn _ => PPM.read input)
val _ = print ("read image in " ^ Time.fmt 4 tm ^ "s\n")

(*
val w = #width image
val h = #height image

fun noisy i =
  let
    val data = Seq.map (fn x => x) (#data image)
  in
    (* spit on 10% of all pixels *)
    Util.for (0, Seq.length data div 10) (fn j =>
      let
        val k = Util.hash (i * Seq.length data + j) mod Seq.length data
      in
        ArraySlice.update (data, k, Color.red)
      end);
    {width = w, height = h, data = data}
  end

val (_, tm) = Util.getTime (fn _ =>
  GIF.writeMany output { width = w
                       , height = h
                       , numImages = 10
                       , getImage = noisy
                       })
*)

val (_, tm) = Util.getTime (fn _ => GIF.write output image)

val _ = print ("wrote " ^ output ^ " in " ^ Time.fmt 4 tm ^ "s\n")
