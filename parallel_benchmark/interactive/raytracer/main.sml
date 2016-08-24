structure RT = IdRaytracer(TreeSequenceG)

open RT

val mirror = [RT.Reflect 1.0]
val sphere = (Ambient (0.035,0.0325,0.025)) :: (Diffuse(0.5,0.45,0.35)) ::
             (Specular(0.8,0.8,0.8)) :: (Specpow 3.0) :: nil

val s1pos = (1.0, 1.0, 1.0)
val s2pos = (~1.0, ~1.0, 1.0)

val world = [Sphere(s1pos, 0.1, sphere), Sphere(s2pos, 0.1, sphere)]

val lookfrom = (2.1, 1.3, 1.7)
val lookat = (0.0, 0.0, 0.0)

val lights = Point((4.0,3.0,2.0), (0.288675,0.288675,0.288675)) ::
             Point((1.0, ~4.0,4.0), (0.288675,0.288675,0.288675)) ::
             Point((~3.0,1.0,5.0), (0.288675,0.288675,0.288675)) :: nil;

val winsize = 400

val () = Graphics.openwindow NONE (winsize, winsize)

val () = MLX.usleep 50000

val () = render lookfrom lookat world lights winsize

fun loop () = loop ()

val () = loop ()
