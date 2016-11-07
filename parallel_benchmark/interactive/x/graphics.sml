(* High-level graphics library *)
(* Uses MLton-specific FFI for X Window System interface *)

(* Stefan Muller *)

structure Graphics : GRAPHICS =
struct

exception NoWindowOpen

type color = MLX.color
type button = MLX.button
type event = MLX.event
type font = MLX.font
type modifier = MLX.modifier

val dwgc : (MLX.display * MLX.window * MLX.gc) option ref = ref NONE
val size = ref (0, 0)
val fg = ref 0wx000000
val bg = ref 0wxffffff

fun openwindow (name : string option) ((wi, h) : int * int) : unit =
    let val d = MLX.opendisplay name
        val rt = MLX.defaultrootwindow d
        val w = MLX.createsimplewindow d rt 5 5 wi h 1 0wx000000 0wxffffff
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
    in
        size := (wi, h);
        dwgc := SOME (d, w, gc)
    end

fun setforeground (c: color) : unit =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, _, gc) =>
        (MLX.setforeground d gc c;
         fg := c)

fun flush () : unit =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, _, _) => MLX.flush d

fun closewindow () : unit =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, _, gc) => (MLX.freegc d gc;
                            MLX.closedisplay d)

fun selectinput (m: MLX.Mask.mask) : unit =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, w, _) => MLX.selectinput d w m

fun nextevent () : event =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, _, _) => #4 (MLX.nextevent d)

fun maskevent (m: MLX.Mask.mask) : event =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, _, _) => #4 (MLX.maskevent d m)

fun checkmaskevent (m: MLX.Mask.mask) : event option =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, _, _) =>
        (case MLX.checkmaskevent d m of
             SOME (_, _, _, e) => SOME e
           | NONE => NONE)

fun mousepos () : int * int =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, w, _) => MLX.mousepos d w

fun buttons () : button list =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, w, _) => MLX.buttons d w

fun button () : bool =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, w, _) => MLX.button d w

fun modstate () : modifier list =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, w, _) => MLX.modstate d w

fun readkey () : char =
    let val old_usec = MLX.get_usechar ()
        val _ = MLX.usechar true
        fun rk_rec () =
            case maskevent (MLX.Mask.make [MLX.Mask.keypress]) of
                MLX.KeyC (true, _, _, _, _, _, _, _, _, _, c, _) => c
              | _ => rk_rec ()
    in
        rk_rec () before MLX.usechar old_usec
    end

fun keyoption () : char option =
    let val old_usec = MLX.get_usechar ()
        val _ = MLX.usechar true
        fun ko_rec () =
            case checkmaskevent (MLX.Mask.make [MLX.Mask.keypress]) of
                NONE => NONE
              | SOME (MLX.KeyC (true, _, _, _, _, _, _, _, _, _, c, _)) => SOME c
              | _ => ko_rec ()
    in
        ko_rec () before MLX.usechar old_usec
    end

fun drawtext (f: font option) (x: int) (y: int) (text: string) =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, w, gc) => MLX.drawtext d (MLX.wd w) gc f x y text

fun drawpoint (x: int) (y: int) =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, w, gc) => MLX.drawpoint d (MLX.wd w) gc x y

fun fillrectangle (x: int) (y: int) (w: int) (h: int) =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, win, gc) => MLX.fillrectangle d (MLX.wd win) gc x y w h

fun drawrectangle (x: int) (y: int) (w: int) (h: int) =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, win, gc) => MLX.drawrectangle d (MLX.wd win) gc x y w h

fun drawline (x1: int) (y1: int) (x2: int) (y2: int) =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, w, gc) => MLX.drawline d (MLX.wd w) gc x1 y1 x2 y2

fun drawrgbarray (x: int) (y: int) (w: int) (h: int) (arr: char Array.array) =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, win, gc) => MLX.drawrgbarray d (MLX.wd win) gc x y w h arr

fun clear () =
    let val (w, h) = !size
        val oldfg = !fg
    in
        setforeground (!bg);
        fillrectangle 0 0 w h;
        setforeground oldfg
    end

end
