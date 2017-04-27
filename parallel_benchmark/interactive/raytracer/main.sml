structure RT = IdRaytracer(TreeSequenceG)

open RT
open IO

val mirror = (* [Ambient (0.8, 0.8, 0.8), Diffuse (0.8, 0.8, 0.8),
              Specular (0.8, 0.8, 0.8), Specpow 3.0, Reflect 0.8] *)
    [Ambient (0.04,0.04,0.04), Diffuse (0.05,0.05,0.05),
     Specular (0.8,0.8,0.8), Specpow 60.0, Reflect 1.0]
val sphere = (Ambient (0.035,0.0325,0.025)) :: (Diffuse(0.5,0.45,0.35)) ::
             (Specular(0.8,0.8,0.8)) :: (Specpow 3.0) :: nil


val atom = MLton.Thread.atomically
val _ = print "really starting\n"

val r = 100000.0
val s3pos = (0.0, 0.0, r)

val lights = Point((4.0,3.0,~2.0), (0.288675,0.288675,0.288675)) ::
             Point((1.0, ~4.0,~4.0), (0.288675,0.288675,0.288675)) ::
             Point((~3.0,1.0,~5.0), (0.288675,0.288675,0.288675)) :: nil;

val s2 = (Ambient (0.035,0.0325,0.025)) :: (Diffuse(0.5,0.45,0.35)) ::
         (Specular(0.8,0.8,0.8)) :: (Specpow 3.0) :: (Reflect 0.5) :: nil;
val s3 = (Ambient (0.1,0.0,0.0)) :: (Diffuse (0.3,0.0,0.0)) ::
         (Specular (0.8,0.4,0.4)) :: (Transmit 0.7) :: nil;

(*
fun readFromFile () =
    let
        val f = TextIO.openIn "spheres.txt"
        fun rd d = Option.valOf (Real.fromString d)
        fun lp acc =
            (case TextIO.inputLine f
              of NONE => List.rev acc
               | SOME line =>
                 let
                     val x::y::z::r::nil = List.map rd (String.tokens (fn c => c = #" ") line)
                 in
                     lp(Sphere((x,y,z),r, s2) :: acc)
                 end)
    in
        lp nil
    end

val testspheres =
    Sphere((0.0,0.0,0.0), 0.5, s3) ::
    Sphere((0.272166,0.272166,0.544331), 0.166667, s2) ::
    Sphere((0.643951,0.172546,0.0), 0.166667, s2) ::
    Sphere((0.172546,0.643951,0.0), 0.166667, s2) ::
    Sphere(((~0.371785),0.0996195,0.544331), 0.166667, s2) ::
    Sphere(((~0.471405),0.471405,0.0), 0.166667, s2) ::
    Sphere(((~0.643951),(~0.172546),0.0), 0.166667, s2) ::
    Sphere((0.0996195,(~0.371785),0.544331), 0.166667, s2) ::
    Sphere(((~0.172546),(~0.643951),0.0), 0.166667, s2) ::
    Sphere((0.471405,(~0.471405),0.0), 0.166667, s2) :: (readFromFile ());
*)

val winsize = 400

val _ = print "starting\n"

val () = atom (fn _ => Graphics.openwindow NONE (winsize, winsize))

(* val () = MLX.usleep 50000 *)

val start = Time.now ()
val frames = ref 0

val lookfrom = ref (5.0, 0.0, ~1.0)
val ang = ref (0.0 - Math.pi)

val stdin = ref (TextIO.getInstream TextIO.stdIn)

fun inputLine () =
    let fun iL_int line =
            case IO.input1 (!stdin) of
                NONE => NONE
              | SOME (c, is') =>
                (stdin := is';
                 if c = #"\n" then
                     SOME line
                 else
                     iL_int (line ^ (str c)))
    in
        iL_int ""
    end

fun inploop () =
    let val (lx, ly, lz) = !lookfrom
        val an = !ang
        (* val _ = MLX.usechar true *)
        val (ax, ay, az) = (lx + Math.cos an, ly + Math.sin an, lz)
        val (l, a) =
(*
            case Graphics.maskevent (MLX.Mask.make [MLX.Mask.keypress]) of
              MLX.KeyC (_, _, _, _, _, _, _, _, _, _, c, _) =>
              (print ("key on " ^ (Int.toString (MLton.Parallel.Basic.processorNumber ())) ^ "\n");
               case c of
                   #"w" => ((lx + (ax - lx) * 0.1, ly + (ay - ly) * 0.1, lz),
                            an)
                 | #"s" => ((lx - (ax - lx) * 0.1, ly - (ay - ly) * 0.1, lz),
                            an)
                 | #"a" => (!lookfrom, an + 0.1)
                 | #"d" => (!lookfrom, an - 0.1)
                 | _ => (!lookfrom, an))
              | _ => (!lookfrom, an)
*)
            case inputLine () of
                   SOME "w" => ((lx + (ax - lx) * 0.1, ly + (ay - ly) * 0.1, lz),
                            an)
                 | SOME "s" => ((lx - (ax - lx) * 0.1, ly - (ay - ly) * 0.1, lz),
                            an)
                 | SOME "a" => (!lookfrom, an + 0.1)
                 | SOME "d" => (!lookfrom, an - 0.1)
                 | _ => (!lookfrom, an)
    in
        lookfrom := l;
        ang := a;
        print "Moved camera\n";
        inploop ()
    end

fun loop s1pos s2pos vel =
    let val world = [Sphere(s1pos, 0.1, sphere), Sphere(s2pos, 0.1, sphere)
                     , Sphere(s3pos, r, mirror)] (*  testspheres *)
        val (x1, y1, z1) = s1pos
        val (x2, y2, z2) = s2pos
        val (z', vel') = if z1 >= ~0.05 then (~0.051, 0.0 - vel * 0.9) else
                         (z1 + vel, vel + 0.01)
        val now = Time.now ()
        val (lx, ly, lz) = !lookfrom
        val (ax, ay, az) = (lx + Math.cos (!ang), ly + Math.sin (!ang), lz)
        val s = LargeInt.toInt (Time.toSeconds (Time.- (now, start)))
    in
        (* (if s > 0 then
             print ((Int.toString (Int.div (!frames, s))) ^ " fps\n")
         else ()); *)
        (* print "yielding\n"; *)
        frames := (!frames) + 1;
        (render (lx, ly, lz) (ax, ay, az) world lights winsize
         handle e => (print "here 143\n"; raise e));
        (* print ("frame on " ^ (Int.toString (MLton.Parallel.Basic.processorNumber ())) ^ "\n"); *)
        (* print ((Int.toString (LargeInt.toInt (Time.toMilliseconds (Time.-
                                                                   (Time.now (),
                                                                    now)))))
                   ^ "\n"); *)
        (* Graphics.flush (); *)
        loop (x1, y1, z') (x2, y2, z') vel'
    end

val _ = MLton.Parallel.ForkJoin.forkLat false
            (inploop,
             (fn _ => loop (1.0, 1.0, ~1.0) (~1.0, ~1.0, ~1.0) 0.0))
