structure RT = IdRaytracer(TreeSequenceG)

open RT
open IO

val mirror = [Ambient (0.8, 0.8, 0.8), Diffuse (0.8, 0.8, 0.8),
              Specular (0.8, 0.8, 0.8), Specpow 3.0, Reflect 0.8]
val sphere = (Ambient (0.035,0.0325,0.025)) :: (Diffuse(0.5,0.45,0.35)) ::
             (Specular(0.8,0.8,0.8)) :: (Specpow 3.0) :: nil


val r = 100000.0
val s3pos = (0.0, 0.0, r)

val lights = Point((4.0,3.0,~2.0), (0.288675,0.288675,0.288675)) ::
             Point((1.0, ~4.0,~4.0), (0.288675,0.288675,0.288675)) ::
             Point((~3.0,1.0,~5.0), (0.288675,0.288675,0.288675)) :: nil;

val winsize = 400

val () = Graphics.openwindow NONE (winsize, winsize)

val () = MLX.usleep 50000

fun loop lookfrom ang s1pos s2pos vel =
    let val world = [Sphere(s1pos, 0.1, sphere), Sphere(s2pos, 0.1, sphere)
                     , Sphere(s3pos, r, mirror)]
        val (x1, y1, z1) = s1pos
        val (x2, y2, z2) = s2pos
        val (z', vel') = if z1 >= ~0.05 then (~0.051, 0.0 - vel * 0.9) else
                         (z1 + vel, vel + 0.01)
        val (lx, ly, lz) = lookfrom
        val (ax, ay, az) = (lx + Math.cos ang, ly + Math.sin ang, lz)
        val (lookfrom', ang') =
            case Graphics.keyoption () of
                NONE => (lookfrom, ang)
              | SOME #"w" => ((lx + (ax - lx) * 0.1, ly + (ay - ly) * 0.1, lz),
                              ang)
              | SOME #"s" => ((lx - (ax - lx) * 0.1, ly - (ay - ly) * 0.1, lz),
                              ang)
              | SOME #"a" => (lookfrom, ang + 0.1)
              | SOME #"d" => (lookfrom, ang - 0.1)
              | _ => (lookfrom, ang)
    in
        IO.yield ();
        render lookfrom (ax, ay, az) world lights winsize;
        Graphics.flush ();
        loop lookfrom' ang' (x1, y1, z') (x2, y2, z') vel'
    end

val () = loop (5.0, 0.0, ~1.0) (0.0 - Math.pi) (1.0, 1.0, ~1.0) (~1.0, ~1.0, ~1.0) 0.0
