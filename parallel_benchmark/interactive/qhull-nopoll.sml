open IO
open Graphics

val _ = openwindow NONE (800, 800)

val _ = IO.yield ()

structure QHull = QuickHull (GeometryArgPar)
fun toList a =
    List.map (fn (x, y) => (Real.floor x, Real.floor y))
             (GeometryArgPar.fold 1000000 op@ (fn x => x) op:: [] a)
fun fromList l =
    GeometryArgPar.fromList 1000000
                            (List.map (fn (x, y) =>
                                          (Real.fromInt x, Real.fromInt y))
                                      l)

fun fib n () =
    if n <= 1 then 1
    else let fun fork (f, g) = if n <= 20 then (f (), g ()) else
                               MLton.Parallel.ForkJoin.fork (f, g)
             val (r1, r2) =
                 fork (fib (n-1), fib (n-2))
         in r1 + r2
         end

fun add p a =
    GeometryArgPar.append 1000000 (a, fromList [p])

fun drawpoint newpt x y =
    (Graphics.drawpoint x y;
     (case newpt of
          NONE => ()
        | SOME (x0, y0) => if x = x0 andalso y = y0
                           then print ("point (" ^ (Int.toString x) ^ ", " ^
                                       (Int.toString y) ^ ")\n")
                           else ()))

fun drawbigpoint x y =
    Graphics.fillrectangle x y 2 2

fun drawlines pts =
    case pts of
      (x1, y1)::(x2, y2)::t =>
      (IO.Graphics.drawline x1 y1 x2 y2;
       drawlines ((x2, y2)::t))
     | _ => ()

fun qhull pts =
    if GeometryArgPar.size pts < 3 then ()
    else
        let val _ = print "starting qhull\n"
            val hullpts = toList (QHull.hull 1000000 pts)
            val _ = print "hull\n"
            val hullpts' = hullpts @ [List.hd hullpts]
        in
            Graphics.clear ();
            List.app (fn (x, y) => drawbigpoint x y) (toList pts);
            drawlines hullpts';
            Graphics.flush ()
        end

fun dedup pts =
    case pts of
     [] => []
     | x::t => x::(List.filter (fn y => x <> y) t)

fun loop b pts =
    let val _ = IO.yield ()
        val click = IO.Graphics.button ()
        (* val _ = print ((Bool.toString click) ^ "\n") *)
        val pts' =
            if click andalso (not b) then
                let val (x, y) = IO.Graphics.mousepos ()
                    val _ = print ("point (" ^ (Int.toString x) ^ ", " ^
                                   (Int.toString y) ^ ")\n")
                    val _ = drawbigpoint x y
                    val _ = Graphics.flush ()
                    val pts' = add (x, y) pts
                    val f = MLton.Parallel.FutureSuspend.future
                                (fn () => qhull pts')
                in
                    pts'
                end
            else pts
    in
        loop click pts'
    end

val _ = MLton.Random.srand (case MLton.Random.seed () of
                                SOME s => s
                              | NONE => 0w0)

fun randint () =
    (* Random.randomInt 800 *)
    (Word.toInt (Word.mod (MLton.Random.rand (), 0w750))) + 25
fun randang () =
    let val b = Word.toInt (Word.mod (MLton.Random.rand (), 0w3000))
        val r = Real.fromInt b
    in
        (r / 3000.0) * 2.0 * Math.pi
    end
val r = 400.0
val cx = 400.0
val cy = 400.0
fun randpt () =
    let val th = randang ()
        val x = randint () (* cx + r * Math.cos th *)
        val y = randint () (* cy + r * Math.sin th *)
    in
        (x, y) (* (Real.floor x, Real.floor y) *)
    end

fun randpoints n =
    if n = 0 then [] else
    (randpt ())::(randpoints (n - 1))

val _ = print "Starting\n"

val ptsl = [] (* randpoints 1000000 *)
val pts = fromList ptsl (* Array.tabulate (3500000, randpt) *)

val f = MLton.Parallel.FutureSuspend.future (fn () => (print "starting\n";
                                                       qhull pts
                                                       before (print "done\n")))

val _ = List.app (fn (x, y) => drawbigpoint x y) ptsl
val _ = MLton.Parallel.Basic.event (fn () => loop false pts)
