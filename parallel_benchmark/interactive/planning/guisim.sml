structure Gui :> WORLD =
struct

structure GS = ChunkedTreeSequence
structure V = Vector

type t =
     {width: int,
      height: int,
      map: bool -> obst,
      graph: graph,
      pts: (int * int) V.vector,
      pixels: bool V.vector V.vector,
      obsts: obst GS.seq,
      pos: real * real,
      long_plan: (int * int) list,
      short_plan: (int * int) list}

val speed = 0.1

val startTime = Time.now ()

fun initFromBmp s =
    let val {width, height, pixels} = Bmp.open_bmp s
        fun iszero (r, g, b) = r = 0w0 andalso g = 0w0 andalso b = 0w0
        fun ispt (r, g, b) =
            (print ("(" ^ (Word8.toString r) ^ ", " ^ (Word8.toString g) ^ ", "
                    ^ (Word8.toString b) ^ ")\n");
            if r < 0wx0f andalso g < 0wx0f andalso b > 0wxf0 then
                (print "pt\n"; true)
            else false)
        fun isobst bumper (x, y) =
            iszero (V.sub (V.sub (pixels, y), x)) orelse
            (bumper andalso
             (iszero (V.sub (V.sub (pixels, y - 1), x)) orelse
              iszero (V.sub (V.sub (pixels, y + 1), x)) orelse
              iszero (V.sub (V.sub (pixels, y), x - 1)) orelse
              iszero (V.sub (V.sub (pixels, y - 1), x - 1)) orelse
              iszero (V.sub (V.sub (pixels, y + 1), x - 1)) orelse
              iszero (V.sub (V.sub (pixels, y), x + 1)) orelse
              iszero (V.sub (V.sub (pixels, y - 1), x + 1)) orelse
              iszero (V.sub (V.sub (pixels, y + 1), x + 1))))
        fun map bumper ((x1, y1), (x2, y2)) =
            if isobst bumper (x1, y1) orelse isobst bumper (x2, y2) then
                true
            else if Int.abs (x1 - x2) <= 1 andalso Int.abs (y1 - y2) <= 1 then
                false
            else
                let val piv = (x1 + Int.div (x2 - x1, 2),
                               y1 + Int.div (y2 - y1, 2))
                in
                    map bumper ((x1, y1), piv) orelse map bumper (piv, (x2, y2))
                end
        fun dist (x1, y1) (x2, y2) =
            let val (x1, y1) = (Real.fromInt x1, Real.fromInt y1)
                val (x2, y2) = (Real.fromInt x2, Real.fromInt y2)
            in
                Math.sqrt ((Math.pow (x1 - x2, 2.0)) + (Math.pow (y1 - y2, 2.0)))
            end
(*
        val gran = 100
        fun pt i =
            let val y = (Int.div (i, Int.div (width, gran))) * gran
                val x = (Int.mod (i, Int.div (width, gran))) * gran
            in
                (x, y)
            end
        val graph =
            let val pts = (Int.div (width, gran)) * (Int.div (height, gran))
                fun f i =
                    let val l = List.tabulate (pts, fn i => i)
                        val l' = List.filter (fn j => not (map (pt i, pt j))) l
                    in
                        V.tabulate
                            (List.length l',
                             fn n => let val j = List.nth (l', n)
                                     in
                                         (dist (pt i) (pt j), j)
                                     end)
                    end
            in
                V.tabulate (pts, f)
            end
*)
        fun findpts y (x, p, l) = if ispt p then (x, y)::l else l
        val pts = V.fromList (V.foldli
                                  (fn (y, v, l) =>
                                      (V.foldli (findpts y) [] v) @ l)
                                  []
                                  pixels)
        val _ = print ((Int.toString (V.length pts)) ^ "\n")
        fun pt i = V.sub (pts, i)
        val graph =
            let val N = V.length pts
                fun f (x, y) =
                    V.fromList
                        (V.foldli
                             (fn (i, (x', y'), l) =>
                                 if (x = x' andalso y = y') orelse
                                    map true ((x, y), (x', y'))
                                 then
                                     l
                                 else
                                     ((*Graphics.drawline x (height - y)
                                                            x' (height - y');*)
                                      (dist (x, y) (x', y'), i)::l)
                             )
                             []
                             pts)
            in
                V.map f pts
            end
    in
        {width = width,
         height = height,
         map = map,
         graph = graph,
         pts = pts,
         obsts = GS.empty (),
         pos = (125.0, 500.0),
         pixels = V.map (V.map iszero) pixels,
         long_plan = [],
         short_plan = []}
    end

fun drawPath height p =
    case p of
        [] => ()
      | [x] => ()
      | (x1, y1)::(x2, y2)::p' =>
        (Graphics.drawline x1 (height - y1) x2 (height - y2);
         drawPath height ((x2, y2)::p'))

fun drawWorld (ow: t option)
              ({width, height, pixels, pos, short_plan, long_plan, ...} : t) =
    let fun noop () = print ""
        fun draw (x, y) =
            if y >= height then (Graphics.flush (); 0)
            else if x >= width then draw (0, y + 1)
            else
                ((* print ("(" ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ ")\n"); *)
                  (* print ""; *)
                  noop ();
                  if V.sub (V.sub (pixels, y), x) then
                      (Graphics.drawpoint x (height - y);
                       (draw (x + 1, y)) + 1)
                  else
                      (draw (x + 1, y)) + 1)
        fun drawpt (x, y) =
            (Graphics.drawpoint x (height - y);
             Graphics.drawpoint (x + 1) (height - y);
             Graphics.drawpoint x (height - y + 1);
             Graphics.drawpoint (x + 1) (height - y + 1))
        val (x, y) = pos
        val ipos = (Real.round x, Real.round y)
    in
        (case ow of
             NONE => ignore (draw (0, 0))
           | SOME ow =>
             let val (ox, oy) = #pos ow
                 val oipos = (Real.round ox, Real.round oy)
             in
                 Graphics.setforeground (MLX.fromints 255 255 255);
                 drawPath height (oipos::(#short_plan ow));
                 drawPath height (#long_plan ow);
                 drawpt oipos;
                 Graphics.setforeground (MLX.fromints 0 0 0)
             end
           );
        Graphics.setforeground (MLX.fromints 255 0 0);
        drawPath height (ipos::short_plan);
        Graphics.setforeground (MLX.fromints 0 0 255);
        drawPath height long_plan;
        Graphics.setforeground (MLX.fromints 0 0 0);
        drawpt ipos;
        Graphics.flush ();
        ignore (Posix.Process.sleep (Time.fromMilliseconds 100))
    end

fun init () =
    let val _ = Graphics.openwindow NONE (504, 891) (* (#width w, #height w) *)
        val w = initFromBmp "gates.bmp"
    in
        drawWorld NONE w;
        w
    end

fun dim ({width, height, ...}: t) = (width, height)
fun map ({graph, ...}: t) = graph
fun pts ({pts, ...} : t) = pts
fun obst ({map, obsts, ...}: t) =
    GS.append (GS.singleton (map true), obsts)

fun pos ({pos, ...}: t) = (Real.round (#1 pos), Real.round (#2 pos))

fun newpos (x, y) (dx, dy) d =
    let val (dx, dy) = (Real.fromInt dx, Real.fromInt dy)
        val dist = Math.sqrt (Math.pow (dx, 2.0) + Math.pow (dy, 2.0))
    in
        if dist < 1.0 then
            (0.0, 0.0)
        else (x + dx * d / dist, y + dy * d / dist)
    end

fun sense_int map obsts (width, height) (x, y) (nx, ny) =
    (* out of bounds *)
    (nx >= (Real.fromInt width) orelse nx < 0.0 orelse
     ny >= (Real.fromInt height) orelse ny < 0.0)
    orelse
    (let val (x, y) = (Real.round x, Real.round y)
         val (nx, ny) = (Real.round nx, Real.round ny)
     in
         (* collision with walls *)
         (map false ((x, y), (nx, ny))) orelse
         (GS.reduce (fn (a, b) => a orelse b)
                    false
                    (GS.map (fn ob => ob ((x, y),
                                          (nx, ny)))
                            obsts))
     end)

fun sense (w as {width, height, pos, map, obsts, ...}: t) (dir, dist) =
    let val (nx, ny) = newpos pos dir dist
    in
        sense_int map obsts (width, height) pos (nx, ny)
    end

fun move (ow as {width, height, map, graph, pts, obsts, pos, pixels, long_plan,
           short_plan }: t) (dx, dy) =
    let val (x, y) = pos
        val (nx, ny) = newpos pos (dx, dy) speed
        val (nx, ny) = if sense_int map obsts (width, height) pos (nx, ny) then
                           (x, y)
                       else
                           (nx, ny)
        val nw = {width = width,
                  height = height,
                  map = map,
                  graph = graph,
                  pts = pts,
                  obsts = obsts,
                  pos = (nx, ny),
                  pixels = pixels,
                  long_plan = long_plan,
                  short_plan = short_plan}
    in
        (if (Real.round nx <> Real.round x) orelse
            (Real.round ny <> Real.round y)
         then
             drawWorld (SOME ow) nw
         else ());
        nw
    end

fun time _ =
    LargeInt.toInt (LargeInt.- (Time.toMilliseconds (Time.now ()),
                                Time.toMilliseconds startTime))

fun register_short_plan (ow as {width, height, map, graph, pts, obsts, pos,
                                pixels, long_plan, short_plan }: t) nsp =
    let val nw = {width = width,
                  height = height,
                  map = map,
                  graph = graph,
                  pts = pts,
                  obsts = obsts,
                  pos = pos,
                  pixels = pixels,
                  long_plan = long_plan,
                  short_plan = nsp}
    in
        drawWorld (SOME ow) nw;
        nw
    end

fun register_long_plan (ow as {width, height, map, graph, pts, obsts, pos,
                               pixels, long_plan, short_plan }: t) nlp =
    let val nw = {width = width,
                  height = height,
                  map = map,
                  graph = graph,
                  pts = pts,
                  obsts = obsts,
                  pos = pos,
                  pixels = pixels,
                  long_plan = nlp,
                  short_plan = short_plan}
    in
        drawWorld (SOME ow) nw;
        nw
    end

end
