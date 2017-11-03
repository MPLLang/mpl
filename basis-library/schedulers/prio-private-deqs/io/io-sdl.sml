structure IOSDL (* : MLTON_PARALLEL_IO *) =
struct

fun input1 (is: TextIO.StreamIO.instream) : (char * TextIO.StreamIO.instream) option =
    let fun f () =
            ((* print "in f\n"; *)
            (case TextIO.StreamIO.canInput (is, 1) of
                 NONE => false
               | SOME _ => true)
            (* before
            print "out f\n" *))
        (* val _ = print ("input1 on " ^ (Int.toString (B.processorNumber ())) ^ "\n") *)
    in
        if f () then
            TextIO.StreamIO.input1 is
        else
            (* Performing the input would block *)
            ((* print "calling suspend\n"; *)
             suspendIO f;
             input1 is)
    end

fun print s = MLton.Thread.atomically (fn () => TextIO.print s)

fun yield () =
    let val ready = ref false
        fun f () = true
(*            let val r = !ready
                val _ = ready := true
            in
                r
            end
*)
    in
        suspendIO f
    end

fun sleep t =
    let val cur = Time.now ()
        val t' = Time.+ (cur, t)
        fun f () = Time.> (Time.now (), t')
    in
        suspendIO f
    end

structure Graphics (* : GRAPHICS *) =
struct

exception NoWindowOpen
exception Unsupported

type color = int * int * int
type button = MLX.button
type event = MLX.event
type font = MLX.font
type modifier = MLX.modifier
type renderer = MLton.Pointer.t

val surf = ref NONE
val size = ref (0, 0)
val fg = ref (255, 255, 255)
val bg = ref (0, 0, 0)
(*
val SDL_CreateSoftwareRenderer = _import "mltSDL_CreateSoftwareRenderer" :
                                 SDLSurface.t -> renderer;
*)
val rend = ref NONE

fun openwindow (name : string option) ((wi, h) : int * int) : unit =
    let val s = SDL_SetVideoModeDefault (wi, h, 32)
        (* val r = SDL_CreateSoftwareRenderer s *)
    in
        size := (wi, h);
        surf := SOME s
        (* rend := SOME r *)
    end

fun setforeground (c: color) : unit =
    (fg := c
(*
     case !rend of
         NONE => ()
       | SOME rd =>
         let val f = _import "mltSDL_SetRenderDrawColor" : renderer * int * int * int * int -> int;
             val (r, g, b) = c
         in
             ignore (f (rd, r, g, b, 255))
         end
*)
)


fun flush () : unit =
    case !surf of
        NONE => raise NoWindowOpen
      | SOME s =>
        let val (w, h) = !size
        in
            SDL_UpdateRect (s, 0, 0, w, h)
        end

fun closewindow () : unit =
    SDL_Quit ()

fun selectinput (m: MLX.Mask.mask) : unit = () (* XXX *)

fun checkmaskevent (m: MLX.Mask.mask) : event option =
    raise Unsupported

fun nextevent () : event =
    raise Unsupported

fun maskevent (m: MLX.Mask.mask) : event =
    raise Unsupported

fun mousepos () : int * int =
    (HcP_GetMouseX (), HcP_GetMouseY ())

fun buttons () : button list =
    raise Unsupported

fun button () : bool =
    let
        val (code, value) = HcP_PollEvent ()
    in
        if code = SDL_MOUSEBUTTONDOWN then
            true
        else
            if code = 0 then false
            else
                button ()
    end

fun modstate () : modifier list =
    raise Unsupported

fun keyoption () : char option =
    raise Unsupported

fun readkey () : char =
    raise Unsupported

fun drawtext (f: font option) (x: int) (y: int) (text: string) =
    raise Unsupported

fun drawpoint (x: int) (y: int) =
    case !surf of
        NONE => raise NoWindowOpen
      | SOME s =>
        let val (r, g, b) = !fg
            (* val f = _import "mltSDL_RenderDrawPoint" : renderer * int * int -> int; *)
        in
            (* ignore (f (rd, x, y)) *)
            HcP_PutPixel (s, r, g, b, x, y)
        end

fun putpixels (pixels: int Array.array) (len: int) =
    case !surf of
        NONE => raise NoWindowOpen
      | SOME s =>
        let val (r, g, b) = !fg
            val f =  _import "PutPixels" : SDLSurface.t * int Array.array * int -> unit;
        in
            ignore (f (s, pixels, len))
        end


fun fillrectangle (x: int) (y: int) (w: int) (h: int) =
    case !surf of
        NONE => raise NoWindowOpen
      | SOME s =>
        let val (r, g, b) = !fg
            val c = MLX.fromints r g b
        in
            ignore (SDL_FillRect (s, x, y, w, h, Word32.toInt c))
        end

fun drawrectangle (x: int) (y: int) (w: int) (h: int) =
    raise Unsupported

fun drawline (x1: int) (y1: int) (x2: int) (y2: int) =
    let val (x1f, y1f) = (Real.fromInt x1, Real.fromInt y1)
        val (x2f, y2f) = (Real.fromInt x2, Real.fromInt y2)
        val d = Math.sqrt(Math.pow (x1f - x2f, 2.0) + Math.pow (y1f - y2f, 2.0))
        val (rf, gf, bf) = !fg
        val (rf, gf, bf) = (Real.fromInt rf, Real.fromInt gf, Real.fromInt bf)
        val (rb, gb, bb) = !bg
        val (rb, gb, bb) = (Real.fromInt rb, Real.fromInt gb, Real.fromInt bb)
        fun draw t =
            let val x = x1f + (x2f - x1f) * t
                val y = y1f + (y2f - y1f) * t
                val {whole = _, frac = fx} = Real.split x
                val {whole = _, frac = fy} = Real.split y
                fun cmb (a, b) = 1.0 - (1.0 - a) * (1.0 - b)
                    (* No antialiasing *)
                    (* if a <= 0.5 andalso b <= 0.5 then 1.0 else 0.0 *)
                val c1 = cmb (1.0 - fx, 1.0 - fy) (* Upper left *)
                val c2 = cmb (fx, 1.0 - fy)       (* Upper right *)
                val c3 = cmb (fx, fy)             (* Lower right *)
                val c4 = cmb (1.0 - fx, fy)       (* Lower left *)
            in
                setforeground (Real.round (rb + (rf - rb) * c1),
                               Real.round (gb + (gf - gb) * c1),
                               Real.round (bb + (bf - bb) * c1));
                drawpoint (Real.floor x) (Real.floor y);
                setforeground (Real.round (rb + (rf - rb) * c2),
                               Real.round (gb + (gf - gb) * c2),
                               Real.round (bb + (bf - bb) * c2));
                drawpoint (Real.ceil x) (Real.floor y);
                setforeground (Real.round (rb + (rf - rb) * c3),
                               Real.round (gb + (gf - gb) * c3),
                               Real.round (bb + (bf - bb) * c3));
                drawpoint (Real.ceil x) (Real.ceil y);
                setforeground (Real.round (rb + (rf - rb) * c4),
                               Real.round (gb + (gf - gb) * c4),
                               Real.round (bb + (bf - bb) * c4));
                drawpoint (Real.floor x) (Real.ceil y)
            end
        val interval = (* 0.5 / d *)
            if abs (y2 - y1) < abs (x2 - x1) then
                           1.0 / (Real.abs (x2f - x1f))
                       else
                           1.0 / (Real.abs (y2f - y1f))
        fun drawall t =
            if t >= 1.0 then ()
            else
                (draw t;
                 drawall (t + interval))
    in
        drawall 0.0;
        setforeground (Real.round rf, Real.round gf, Real.round bf)
    end
(*
    case !rend of
        NONE => raise NoWindowOpen
      | SOME rd =>
        let val (r, g, b) = !fg
            val f = _import "mltSDL_RenderDrawLine" : renderer * int * int * int * int -> int;
        in
            ignore (f (rd, x1, y1, x2, y2))
        end
*)

fun clear () =
    let val (w, h) = !size
        val oldfg = !fg
    in
        setforeground (!bg);
        fillrectangle 0 0 w h;
        setforeground oldfg
    end

end

structure Network : NETWORK =
struct

open Socket

fun B_of_NB (f: 'a -> 'b option) (v: 'a) : 'b =
    let val (r: 'b option ref) = ref NONE
        fun f' () =
            (case !r of
                 NONE => (r := f v;
                          case !r of
                              NONE => false
                            | SOME _ => true)
               | SOME _ => true)
        fun bnb_rec () =
            if f' () then
                case !r of
                    NONE => raise OS.SysErr ("Impossible", NONE)
                  | SOME c => c
            else
                (suspendIO f';
                 bnb_rec ())
    in
        bnb_rec ()
    end

fun accept s = B_of_NB acceptNB s
fun connect s =
    B_of_NB (fn s => if connectNB s then SOME () else NONE) s

fun sendString (sock, str) =
    let val arr = Array.tabulate (String.size str,
                                 fn i => Word8.fromInt (Char.ord (String.sub (str, i))))
        val slice = ArraySlice.slice (arr, 0, NONE)
    in
        B_of_NB sendArrNB (sock, slice)
    end

fun recvString (sock, n) =
    let val v = B_of_NB recvVecNB (sock, n)
    in
        Vector.foldl (fn (c, s) =>
                         s ^ (String.str (Char.chr (Word8.toInt c))))
                     ""
                     v
    end
end

end
