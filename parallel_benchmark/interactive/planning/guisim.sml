structure Gui :> WORLD =
struct

structure GS = ChunkedTreeSequence
structure V = Vector

type t =
     {width: int,
      height: int,
      map: obst,
      graph: graph,
      pts: (int * int) V.vector,
      pixels: bool V.vector V.vector,
      obsts: obst GS.seq,
      pos: real * real,
      long_plan: (int * int) list,
      short_plan: (int * int) list}

val speed = 1.0

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
        fun isobst (x, y) = iszero (V.sub (V.sub (pixels, y), x))
        fun map ((x1, y1), (x2, y2)) =
            if isobst (x1, y1) orelse isobst (x2, y2) then
                true
            else if Int.abs (x1 - x2) <= 1 andalso Int.abs (y1 - y2) <= 1 then
                false
            else
                let val piv = (x1 + Int.div (x2 - x1, 2),
                               y1 + Int.div (y2 - y1, 2))
                in
                    map ((x1, y1), piv) orelse map (piv, (x2, y2))
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
                                    map ((x, y), (x', y'))
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

fun drawWorld {width, height, pixels, pos, short_plan, long_plan, ...} =
    let val _ = Graphics.clear ()
        fun noop () = print ""
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
        draw (0, 0);
        Graphics.setforeground (MLX.fromints 255 0 0);
        drawPath height short_plan;
        Graphics.setforeground (MLX.fromints 0 0 255);
        drawPath height long_plan;
        Graphics.setforeground (MLX.fromints 0 0 0);
        drawpt ipos;
        Graphics.flush ()
    end

fun init () =
    let val _ = Graphics.openwindow NONE (504, 891) (* (#width w, #height w) *)
        val w = initFromBmp "gates.bmp"
    in
        drawWorld w;
        w
    end

fun dim ({width, height, ...}: t) = (width, height)
fun map ({graph, ...}: t) = graph
fun pts ({pts, ...} : t) = pts
fun obst ({map, obsts, ...}: t) =
    GS.append (GS.singleton map, obsts)

fun pos ({pos, ...}: t) = (Real.round (#1 pos), Real.round (#2 pos))
fun move ({width, height, map, graph, pts, obsts, pos, pixels, long_plan,
           short_plan }: t) (dx, dy) =
    let val (x, y) = pos
        val (dx, dy) = (Real.fromInt dx, Real.fromInt dy)
        val dist = Math.sqrt (Math.pow (dx, 2.0) + Math.pow (dy, 2.0))
        val (dx, dy) = if dist < 1.0 then
                           (0.0, 0.0)
                       else (dx * speed / dist, dy * speed / dist)
        val (nx, ny) =
            if x + dx >= (Real.fromInt width) orelse x + dx < 0.0 orelse
               y + dy >= (Real.fromInt height) orelse y + dy < 0.0
            then
                (x, y)
            else if map ((Real.round x, Real.round y),
                         (Real.round (x + dx), Real.round (y + dy))) then
                (x, y)
            else if GS.reduce (fn (a, b) => a orelse b)
                              false
                              (GS.map (fn ob => ob ((Real.round x, Real.round y),
                                                    (Real.round (x + dx), Real.round (y + dy))))
                                      obsts)
            then
                (x, y)
            else
                (x + dx, y + dy)
        val nw = {width = width,
                  height = height,
                  map = map,
                  graph = graph,
                  pts = pts,
                  obsts = obsts,
                  pos = (x + dx, y + dy),
                  pixels = pixels,
                  long_plan = long_plan,
                  short_plan = short_plan}
    in
        (if (Real.round nx <> Real.round x) orelse
            (Real.round ny <> Real.round y)
         then
             drawWorld nw
         else ());
        nw
    end

fun time _ =
    LargeInt.toInt (LargeInt.- (Time.toMilliseconds (Time.now ()),
                                Time.toMilliseconds startTime))

fun register_short_plan ({width, height, map, graph, pts, obsts, pos, pixels,
                          long_plan, short_plan }: t) nsp =
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
        drawWorld nw;
        nw
    end

fun register_long_plan ({width, height, map, graph, pts, obsts, pos, pixels,
                          long_plan, short_plan }: t) nlp =
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
        drawWorld nw;
        nw
    end

end
