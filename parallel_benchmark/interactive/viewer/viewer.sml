open Array

type image = MLton.Pointer.t

val init_img = _import "init_img": string -> image;
val get_width = _import "width": image -> int;
val get_height = _import "height": image -> int;
val decode = _import "decode": image * char array -> bool;
val finish = _import "finish": image -> unit;

val swidth = 600
val sheight = 600
val numtorender = 2

(*
val d = MLX.opendisplay NONE
val rt = MLX.defaultrootwindow d
val w = MLX.createsimplewindow d rt 5 5 width height 1 0wx000000 0wxffffff
val gc = MLX.creategc d (MLX.wd w)
val _ = MLX.mapwindow d w
val _ = MLX.raisewindow d w
val _ = MLX.setforeground d gc 0wx000000
val _ = MLX.flush d
val _ = MLX.selectinput d w (MLX.Mask.make [MLX.Mask.exposure,
                                            MLX.Mask.keyrelease,
                                            MLX.Mask.keypress,
                                            MLX.Mask.buttonpress,
                                            MLX.Mask.buttonrelease,
                                            MLX.Mask.pointermotion])
val _ = MLX.usleep 50000
*)

val stdin = ref (TextIO.getInstream TextIO.stdIn)

fun inputLine () = (* TextIO.inputLine TextIO.stdIn *)
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


fun render file =
    let fun paint arr surf (scale, dx, dy, dw, dh) (w, x, y) =
            if x < dx orelse x > dx + dw orelse y < dy orelse y > dy + dh
            then
                HcP_PutPixel(surf, 0, 0, 0, x, y)
            else
                let val (px, py) =
                        (Real.round ((Real.fromInt (x - dx)) / scale),
                         Real.round ((Real.fromInt (y - dy)) / scale))
                    val i = (py * w + px) * 3
                    val out = i < 0 orelse i >= length arr
                    val r = if out then 0 else Char.ord (sub (arr, i))
                    val g = if out then 0 else Char.ord (sub (arr, i + 1))
                    val b = if out then 0 else Char.ord (sub (arr, i + 2))
            (* val _ = print ((Int.toString x) ^ ", " ^ (Int.toString y) ^ ":" ^
                       (Int.toString r) ^ " " ^ (Int.toString g) ^ " " ^
                       (Int.toString b) ^ "\n") *)
            in
                HcP_PutPixel(surf, r, g, b, x, y)
            (*
        Graphics.setforeground (MLX.fromints r g b)
        handle Overflow =>
               Graphics.setforeground (MLX.fromrgb 0.0 0.0 0.0);
        Graphics.drawpoint x y
            *)
            end

        fun dopaint arr surf (scale, dx, dy, dw, dh) (w, x, y) =
            if x >= swidth then
                if y >= sheight - 1 then
                    ()
                else
                    dopaint arr surf (scale, dx, dy, dw, dh) (w, 0, y + 1)
            else
                ((* print ("Painting " ^ (Int.toString x) ^ ", " ^ (Int.toString y) ^ "\n"); *)
                  paint arr surf (scale, dx, dy, dw, dh) (w, x, y);
                  dopaint arr surf (scale, dx, dy, dw, dh) (w, x + 1, y))

        val img = init_img file
        val width = get_width img
        val height = get_height img
        val sz = width * height * 3
        (* val _ = print ("allocating " ^ (Int.toString sz) ^ " bytes\n") *)
        val arr = array (sz, #"a")

        (* val _ = print "decoding\n" *)
        val _ = decode (img, arr)
        (* val _ = print "decoded\n" *)
        val _ = finish img
        val surf = SDL_AllocSurfaceDefaultAlpha(swidth, sheight, 32,
                                                0, 0, 0, 0)
(*
                                                255,
                                                255 * 256,
                                                255 * 256 * 256,
                                                0)
*)

        val (fsw, fsh) = (Real.fromInt swidth, Real.fromInt sheight)
        val (fw, fh) = (Real.fromInt width, Real.fromInt height)
        val (scale, dw, dh) =
            if width > height then
                let val scale = fsw / fw
                    val (fdw, fdh) = (fsw, fh * scale)
                in
                    (scale, Real.round fdw, Real.round fdh)
                end
            else
                let val scale = fsh / fh
                    val (fdw, fdh) = (fw * scale, fsh)
                in
                    (scale, Real.round fdw, Real.round fdh)
                end
        val (dx, dy) = (Int.div (swidth - dw, 2), Int.div (sheight - dh, 2))
    in
        (* print "painting\n"; *)
        dopaint arr surf (scale, dx, dy, dw, dh) (width, 0, 0);
        (* print "painted\n"; *)
        SDL_UpdateRect(surf, 0, 0, width, height);
        surf
    end

val sdlscreen = SDL_SetVideoModeDefault(swidth, sheight, 32)

fun display surf =
    let
    in
        SDL_BlitSurface (surf, 0, 0, swidth, sheight,
                         sdlscreen, 0, 0, swidth, sheight);
        SDL_UpdateRect (sdlscreen, 0, 0, swidth, sheight)
    end

structure F = OS.FileSys
val dir = F.openDir ((F.getDir ()) ^ "/photos")
fun readjpegs () =
    case F.readDir dir of
        NONE => []
      | SOME f =>
        (if String.isSuffix ".jpg" (String.map Char.toLower f) then
             f::(readjpegs ())
         else
             readjpegs ())
val jpegs = readjpegs ()
val njpegs = List.length jpegs

fun filename n =
(*     (Int.toString n) ^ ".jpg" *)
    "photos/" ^ (List.nth (jpegs, n))

val surfarr = array (njpegs, NONE)

fun renderinback n =
    ((* print ("rendering " ^ (Int.toString n) ^ "\n"); *)
     (case sub (surfarr, n) of
          SOME _ => ()
        | NONE =>
          update (surfarr, n, SOME (render (filename n)))
     )(* ;
     print ("rendered " ^ (Int.toString n) ^ "\n") *))

fun rendernext (n, num) =
    if num = 0 then ()
    else
        ((* print ("spawning " ^ (Int.toString n) ^ "\n"); *)
         ignore (MLton.Parallel.FutureSuspend.future
                     (fn () => renderinback n));
         rendernext (n + 1, num - 1))

(* val _ = rendernext (0, numtorender) *)

fun dispnum n surfarr =
    ((case sub (surfarr, n) of
         NONE =>
         (let val surf = render (filename n) in
              (* print "NEWLY RENDERED\n"; *)
              display surf;
              update (surfarr, n, SOME surf)
          end)
       | SOME surf => display surf);
     rendernext (n, numtorender))

fun loop n =
    let val _ = dispnum n surfarr
        val n' =
            case inputLine () of
                NONE => n
              | SOME "n" => n + 1
              | SOME "p" => n - 1
              | SOME "q" => (SDL_Quit ();
                             OS.Process.exit OS.Process.success)
              | SOME s => (case Int.fromString s of
                          SOME n' => n'
                        | NONE => n)
    in
        loop (n' mod njpegs)
    end

val _ = MLton.Parallel.Basic.event (fn _ => loop 0)
