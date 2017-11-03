structure IO : MLTON_PARALLEL_IO =
struct

fun input1 (is: TextIO.StreamIO.instream) : (char * TextIO.StreamIO.instream) option =
    let fun f () =
            ((* print "in f\n"; *)
            (case TextIO.StreamIO.canInput (is, 1) of
                 NONE => false
               | SOME _ => true)
            (* before
            print "out f\n" *))
        (* val _ = print ("input1 on " ^ (Int.toString (processorNumber ())) ^ "\n") *)
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
        fun f () =
            let val r = !ready
                val _ = ready := true
            in
                r
            end
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
    let val _ = MLX.initthreads ()
        val d = MLX.opendisplay name
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

fun checkmaskevent (m: MLX.Mask.mask) : event option =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, _, _) =>
        (case MLX.checkmaskevent d m of
             SOME (_, _, _, e) => SOME e
           | NONE => NONE)

fun nextevent () : event =
    let val m = MLX.Mask.make [MLX.Mask.keypress,
                           MLX.Mask.keyrelease,
                           MLX.Mask.buttonpress,
                           MLX.Mask.buttonrelease,
                           MLX.Mask.enterwindow,
                           MLX.Mask.leavewindow,
                           MLX.Mask.pointermotion,
                           MLX.Mask.pointermotionhint,
                           MLX.Mask.button1motion,
                           MLX.Mask.button2motion,
                           MLX.Mask.button3motion,
                           MLX.Mask.button4motion,
                           MLX.Mask.button5motion,
                           MLX.Mask.buttonmotion,
                           MLX.Mask.keymapstate,
                           MLX.Mask.exposure,
                           MLX.Mask.visibilitychange,
                           MLX.Mask.structurenotify,
                           MLX.Mask.resizeredirect,
                           MLX.Mask.substructurenotify,
                           MLX.Mask.substructureredirect,
                           MLX.Mask.focuschange,
                           MLX.Mask.propertychange,
                           MLX.Mask.colormapchange,
                           MLX.Mask.ownergrabbutton]
        fun ne_int () =
            let val (k: event option ref) = ref NONE
                fun f () =
                    (k := checkmaskevent m;
                     case !k of
                         NONE => false
                       | SOME _ => true)
            in
                if f () then
                    case !k of
                        NONE => raise NoWindowOpen (* Impossible *)
                      | SOME e => e
                else
                    (suspendIO f;
                     ne_int ())
            end
    in
        case !dwgc of
            NONE => raise NoWindowOpen
          | SOME (d, _, _) => ne_int ()
    end

fun maskevent (m: MLX.Mask.mask) : event =
    let val (k: event option ref) = ref NONE
        fun me_int () =
            let fun f () =
                    case !k of
                        NONE =>
                        (k := checkmaskevent m;
                         case !k of
                             NONE => false
                           | SOME _ => true)
                      | SOME _ => true
            in
                if f () then
                    case !k of
                        NONE => raise NoWindowOpen (* Impossible *)
                      | SOME e => e
                else
                    (suspendIO f;
                     me_int ())
            end
    in
        case !dwgc of
            NONE => raise NoWindowOpen
          | SOME (d, _, _) => me_int ()
    end

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

fun readkey () : char =
    let val (k: char option ref) = ref NONE
        fun f () =
            (k := keyoption ();
             case !k of
                 NONE => false
               | SOME _ => true)
    in
        if f () then
            case !k of
                NONE => raise NoWindowOpen (* Impossible *)
              | SOME c => c
        else
            (suspendIO f;
             readkey ())
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
      | SOME (d, win, gc) => MLX.fillrectangle d (MLX.wd win) gc x y w h

fun drawline (x1: int) (y1: int) (x2: int) (y2: int) =
    case !dwgc of
        NONE => raise NoWindowOpen
      | SOME (d, w, gc) => MLX.drawline d (MLX.wd w) gc x1 y1 x2 y2

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

fun accept s = B_of_NB Socket.acceptNB s
fun connect s =
    B_of_NB (fn s => if Socket.connectNB s then SOME () else NONE) s

fun sendArr s = B_of_NB Socket.sendArrNB s
fun sendVec s = B_of_NB Socket.sendVecNB s
fun recvArr s = B_of_NB Socket.recvArrNB s
fun recvVec s = B_of_NB Socket.recvVecNB s

fun sendString (sock, str) =
    let val arr = Array.tabulate (String.size str,
                                 fn i => Word8.fromInt (Char.ord (String.sub (str, i))))
        val slice = ArraySlice.slice (arr, 0, NONE)
    in
        sendArr (sock, slice)
    end

fun recvString (sock, n) =
    let val v = recvVec (sock, n)
    in
        Vector.foldl (fn (c, s) =>
                         s ^ (String.str (Char.chr (Word8.toInt c))))
                     ""
                     v
    end
end

end
