

(* ========================================================================= *)

val height = CommandLineArgs.parseInt "m" 200
val width = CommandLineArgs.parseInt "n" 200
val grain = CommandLineArgs.parseInt "grain" 256
val f = CommandLineArgs.parseString "f" ""
val scene_name = CommandLineArgs.parseString "s" "rgbbox"
val scene = case scene_name of
                "rgbbox" => Ray.rgbbox
              | "irreg" => Ray.irreg
              | s => raise Fail ("No such scene: " ^ s)

val _ = print ("Using scene '" ^ scene_name ^ "' (-s to switch).\n")

val t0 = Time.now ()
val (objs, cam) = Ray.from_scene width height scene
val t1 = Time.now ()
val _ = print ("Scene BVH construction in " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s.\n")

val t0 = Time.now ()
val result = Ray.render grain objs width height cam
val t1 = Time.now ()

val _ = print ("Rendering in " ^ Time.fmt 4 (Time.- (t1, t0)) ^ "s.\n")

val writeImage = Ray.image2ppm6

val _ = if f <> "" then
            let val out = TextIO.openOut f
            in print ("Writing image to " ^ f ^ ".\n")
               before writeImage out result
               before TextIO.closeOut out
            end
        else print ("-f not passed, so not writing image to file.\n")
