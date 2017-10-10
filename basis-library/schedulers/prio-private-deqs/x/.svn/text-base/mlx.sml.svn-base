

structure MLX :> MLX =
struct

    (* these are all represented as pointers in C *)
    type display = Word32.word option ref
    type window = Word32.word option ref
    type gc = Word32.word option ref

    type pixmap = Word32.word option ref

    type drawable = window

    type modstate = Word32.word
    type time = Word32.word (* XXX *)

    (* this is really just a word *)
    type color = Word32.word

    type serial = int

    (* currently impossible to get fonts *)
    datatype font = Void of font

    fun pd x = x
    fun wd x = x

    datatype button =
        Button1 | Button2 | Button3 | Button4 | Button5

    datatype event = 
        Any of unit
        (* up/down, window, root, subwindow, time, ptrx, ptry, ptrrx, ptrry, state, keycode, same_screen *)
      | Key of bool * window * window * window * time * int * int * int * int * modstate * Word32.word * bool
        (* window, root, subwindow, time ptrx, ptry, ptrrx, ptrry, state, ishint, same_screen *)
      | Motion of window * window * window * time * int * int * int * int * modstate * bool * bool
        (* up/down, window, root, subwindow, time, ptrx, ptry, ptrrx, ptrry, state, button, same_screen *)
      | Button of bool * window * window * window * time * int * int * int * int * modstate * button * bool
      | Crossing of unit
      | FocusChange of unit
        (* window x y width height count *)
      | Expose of window * int * int * int * int * int
      | GraphicsExpose of unit
        (*                     major minor *)
      | NoExpose of drawable * int * int
      | Visibility of unit
      | CreateWindow of unit
      | DestroyWindow of unit
      | Unmap of unit
        (*      event window override_redirect *)
      | Map of window * window * bool
      | MapRequest of window * window
      | Reparent of unit
        (*            event  window    x     y     w     h    border above   override_redirect *)
      | Configure of window * window * int * int * int * int * int * window * bool
      | Gravity of unit
      | ResizeRequest of unit
      | ConfigureRequest of unit
      | Circulate of unit
      | CirculateRequest of unit
      | Property of unit
      | SelectionClear of unit
      | SelectionRequest of unit
      | Selection of unit
      | Colormap of unit
      | ClientMessage of unit
      | Mapping of unit
      | Error of unit
      | Keymap of unit

    exception X of string

    fun oops _ = raise X "bad event type"

    fun motionnotify xe =
        let
            val gkp = _import "ml_getmotion" : Word32.word * 
                Word32.word ref * Word32.word ref * Word32.word ref *
                Word32.word ref *
                int ref * int ref *
                int ref * int ref *
                Word32.word ref *
                bool ref *
                bool ref -> unit ;

            val wnd = ref 0w0
            val root = ref 0w0
            val sub = ref 0w0
            val t = ref 0w0
            val x = ref 0
            val y = ref 0
            val rx = ref 0
            val ry = ref 0
            val state = ref 0w0
            val ih = ref false
            val ss = ref false
        in
            gkp (xe, wnd, root, sub, t, x, y, rx, ry, state, ih, ss);
            Motion (ref (SOME (!wnd)), ref (SOME (!root)), ref (SOME (!sub)), 
                    !t, !x, !y, !rx, !ry, !state, !ih, !ss)
        end

    fun enternotify _ = raise X "unimplemented: enternotify"
    fun leavenotify _ = raise X "unimplemented: leavenotify"
    fun focusin _ = raise X "unimplemented: focusin"
    fun focusout _ = raise X "unimplemented: focusout"
    fun keymapnotify _ = raise X "unimplemented: keymapnotify"
    fun expose xe =
        let
            val ge = _import "ml_getexpose" : Word32.word * Word32.word ref *
                int ref * int ref * int ref * int ref * int ref -> unit ;
            val wnd = ref 0w0
            val x = ref 0
            val y = ref 0
            val w = ref 0
            val h = ref 0
            val c = ref 0
        in
            ge(xe, wnd, x, y, w, h, c);
            Expose (ref (SOME (!wnd)), !x, !y, !w, !h, !c)
        end

    fun graphicsexpose _ = raise X "unimplemented: graphicsexpose"
    fun noexpose xe =
        let
            val gne = _import "ml_getnoexpose" : Word32.word * Word32.word ref * int ref * int ref -> unit ;
            val drw = ref 0w0
            val x   = ref 0
            val y   = ref 0
        in
            gne (xe, drw, x, y);
            NoExpose (ref (SOME (!drw)), !x, !y)
        end

    fun visibilitynotify _ = raise X "unimplemented: visibilitynotify"
    fun createnotify _ = raise X "unimplemented: createnotify"
    fun destroynotify _ = raise X "unimplemented: destroynotify"
    fun unmapnotify _ = raise X "unimplemented: unmapnotify"

    fun mapnotify xe =
        let
            val gmn = _import "ml_getmapnotify" : Word32.word * Word32.word ref * Word32.word ref * bool ref -> unit ;
            val evt = ref 0w0
            val wnd = ref 0w0
            val orr = ref false
        in
            gmn (xe, evt, wnd, orr);
            Map (ref (SOME (!evt)), ref (SOME (!wnd)), !orr)
        end

    fun maprequest xe =
        let
            val gmr = _import "ml_getmaprequest" : Word32.word * Word32.word ref * Word32.word ref -> unit ;
            val par = ref 0w0
            val wnd = ref 0w0
        in
            gmr (xe, par, wnd);
            MapRequest (ref (SOME (!par)), ref (SOME (!wnd)))
        end

    fun keyevent xe =
        let
            val gkp = _import "ml_getkeypress" : Word32.word * 
                bool ref *
                Word32.word ref * Word32.word ref * Word32.word ref *
                Word32.word ref *
                int ref * int ref *
                int ref * int ref *
                Word32.word ref *
                Word32.word ref *
                bool ref -> unit ;
            val isdown = ref false
            val wnd = ref 0w0
            val root = ref 0w0
            val sub = ref 0w0
            val t = ref 0w0
            val x = ref 0
            val y = ref 0
            val rx = ref 0
            val ry = ref 0
            val state = ref 0w0
            val kc = ref 0w0
            val ss = ref false
        in
            gkp (xe, isdown, wnd, root, sub, t, x, y, rx, ry, state, kc, ss);
            Key (!isdown, ref (SOME (!wnd)), ref (SOME (!root)), ref (SOME (!sub)), 
                 !t, !x, !y, !rx, !ry, !state, !kc, !ss)
        end


    fun buttonevent xe =
        let
            val gb = _import "ml_getbutton" : Word32.word * 
                bool ref *
                Word32.word ref * Word32.word ref * Word32.word ref *
                Word32.word ref *
                int ref * int ref *
                int ref * int ref *
                Word32.word ref *
                Word32.word ref *
                bool ref -> unit ;
            val isdown = ref false
            val wnd = ref 0w0
            val root = ref 0w0
            val sub = ref 0w0
            val t = ref 0w0
            val x = ref 0
            val y = ref 0
            val rx = ref 0
            val ry = ref 0
            val state = ref 0w0
            val kc = ref 0w0
            val ss = ref false

            val b1 = _import "MLX_Button1" : Word32.word ;
            val b2 = _import "MLX_Button2" : Word32.word ;
            val b3 = _import "MLX_Button3" : Word32.word ;
            val b4 = _import "MLX_Button4" : Word32.word ;
            val b5 = _import "MLX_Button5" : Word32.word ;
                    
        in
            gb (xe, isdown, wnd, root, sub, t, x, y, rx, ry, state, kc, ss);
            Button (!isdown, ref (SOME (!wnd)), ref (SOME (!root)), ref (SOME (!sub)), 
                    !t, !x, !y, !rx, !ry, !state, 
                    (if !kc = b1 then Button1
                     else if !kc = b2 then Button2
                          else if !kc = b3 then Button3
                               else if !kc = b4 then Button4
                                    else if !kc = b5 then Button5
                                         else raise X "Button not 0..5?"),
                         !ss)
        end

    fun reparentnotify _ = raise X "unimplemented: reparentnotify"
    fun configurenotify xe = 
        let
            val gcn = _import "ml_getconfigurenotify" : Word32.word * 
                Word32.word ref *
                Word32.word ref *
                int ref * int ref * int ref * int ref *
                int ref *
                Word32.word ref * bool ref -> unit ;

            val evt = ref 0w0
            val wnd = ref 0w0
            val x = ref 0
            val y = ref 0
            val w = ref 0
            val h = ref 0
            val b = ref 0
            val abo = ref 0w0
            val orr = ref false
        in
            gcn (xe, evt, wnd, x, y, w, h, b, abo, orr);
            Configure (ref (SOME (!evt)), ref (SOME (!wnd)), !x, !y, !w, !h, !b, ref (SOME (!abo)), !orr)
        end
    fun configurerequest _ = raise X "unimplemented: configurerequest"
    fun gravitynotify _ = raise X "unimplemented: gravitynotify"
    fun resizerequest _ = raise X "unimplemented: resizerequest"
    fun circulatenotify _ = raise X "unimplemented: circulatenotify"
    fun circulaterequest _ = raise X "unimplemented: circulaterequest"
    fun propertynotify _ = raise X "unimplemented: propertynotify"
    fun selectionclear _ = raise X "unimplemented: selectionclear"
    fun selectionrequest _ = raise X "unimplemented: selectionrequest"
    fun selectionnotify _ = raise X "unimplemented: selectionnotify"
    fun colormapnotify _ = raise X "unimplemented: colormapnotify"
    fun clientmessage _ = raise X "unimplemented: clientmessage"
    fun mappingnotify _ = raise X "unimplemented: mappingnotify"

    (* order of this list from X.h; better not change it! *)
    val handlers = Vector.fromList
        [oops,
         oops,
         keyevent,
         keyevent,
         buttonevent,
         buttonevent,
         motionnotify,
         enternotify,
         leavenotify,
         focusin,
         focusout,
         keymapnotify,
         expose,
         graphicsexpose,
         noexpose,
         visibilitynotify,
         createnotify,
         destroynotify,
         unmapnotify,
         mapnotify,
         maprequest,
         reparentnotify,
         configurenotify,
         configurerequest,
         gravitynotify,
         resizerequest,
         circulatenotify,
         circulaterequest,
         propertynotify,
         selectionclear,
         selectionrequest,
         selectionnotify,
         colormapnotify,
         clientmessage,
         mappingnotify ]


    fun closedisplay _ = raise X "unimplemented"
    fun opendisplay _ = raise X "unimplemented"
    fun defaultrootwindow _ = raise X "unimplemented"
    fun creategc _ = raise X "unimplemented"

    structure Mask =
    struct
        type item = Word32.word
        type mask = Word32.word
            
        val noevent = _import "MLX_NoEventMask" : Word32.word ;
        val keypress = _import "MLX_KeyPressMask" : Word32.word ;
        val keyrelease = _import "MLX_KeyReleaseMask" : Word32.word ;
        val buttonpress = _import "MLX_ButtonPressMask" : Word32.word ;
        val buttonrelease = _import "MLX_ButtonReleaseMask" : Word32.word ;
        val enterwindow = _import "MLX_EnterWindowMask" : Word32.word ;
        val leavewindow = _import "MLX_LeaveWindowMask" : Word32.word ;
        val pointermotion = _import "MLX_PointerMotionMask" : Word32.word ;
        val pointermotionhint = _import "MLX_PointerMotionHintMask" : Word32.word ;
        val button1motion = _import "MLX_Button1MotionMask" : Word32.word ;
        val button2motion = _import "MLX_Button2MotionMask" : Word32.word ;
        val button3motion = _import "MLX_Button3MotionMask" : Word32.word ;
        val button4motion = _import "MLX_Button4MotionMask" : Word32.word ;
        val button5motion = _import "MLX_Button5MotionMask" : Word32.word ;
        val buttonmotion = _import "MLX_ButtonMotionMask" : Word32.word ;
        val keymapstate = _import "MLX_KeymapStateMask" : Word32.word ;
        val exposure = _import "MLX_ExposureMask" : Word32.word ;
        val visibilitychange = _import "MLX_VisibilityChangeMask" : Word32.word ;
        val structurenotify = _import "MLX_StructureNotifyMask" : Word32.word ;
        val resizeredirect = _import "MLX_ResizeRedirectMask" : Word32.word ;
        val substructurenotify = _import "MLX_SubstructureNotifyMask" : Word32.word ;
        val substructureredirect = _import "MLX_SubstructureRedirectMask" : Word32.word ;
        val focuschange = _import "MLX_FocusChangeMask" : Word32.word ;
        val propertychange = _import "MLX_PropertyChangeMask" : Word32.word ;
        val colormapchange = _import "MLX_ColormapChangeMask" : Word32.word ;
        val ownergrabbutton = _import "MLX_OwnerGrabButtonMask" : Word32.word ;
            
        val empty = 0w0 : Word32.word
        fun make l = foldl Word32.orb empty l
        fun contains m i = Word32.> (Word32.andb (m, i), 0w0)
        fun set m i = Word32.orb (m, i)
        fun unset m i = Word32.andb(m, Word32.notb i)
    end

    fun selectinput (ref (SOME d)) (ref (SOME w)) m =
        let
            val si = _import "XSelectInput" : Word32.word * Word32.word * Word32.word -> unit ;
        in
            si (d, w, m)
        end
      | selectinput _ _ _ = raise X "selectinput: bad display/window"

    fun opendisplay so  : display =
        let
            (* hehe. I want to pass either 0 ("NULL") or the string,
               so this function can have both types.. *)
            val od  = _import "XOpenDisplay" : string -> Word32.word ;
            val od0 = _import "XOpenDisplay" : int -> Word32.word ;
        in
            (ref (SOME (case (case so of
                                  NONE => od0 0
                                | SOME s => (od (s ^ implode([chr 0])))) of
                            0w0 => raise X "opendisplay failed"
                          | w => w)))
        end

    fun closedisplay ((ref NONE) : display) = raise X "display already closed"
      | closedisplay (r as (ref (SOME d))) : unit =
        let
            (* XXX check return value? *)
            val cd = _import "XCloseDisplay" : Word32.word -> unit ;
        in
            cd d;
            r := NONE
        end

    fun creategc (ref (SOME d)) (ref (SOME w)) : gc =
        let
            val cg = _import "ml_creategc" : Word32.word * Word32.word -> Word32.word ;
        in
            (ref (SOME 
            (case cg (d, w) of
                 0w0 => raise X "creategc failed"
               | ww => ww)))
        end
      | creategc (_ : display) (_ : window) = raise X "creategc: bad display / window"


    fun defaultrootwindow (ref (SOME d))  : window =
        let
            val drw = _import "XDefaultRootWindow" : Word32.word -> Word32.word ;
        in
            (ref (SOME
            (case drw d of
                 0w0 => raise X "defaultrootwindow failed"
               | w => w)))
        end
      | defaultrootwindow (_ : display) = raise X "defaultrootwindow: bad display"

    fun createsimplewindow (ref (SOME d)) (ref (SOME win)) xoff yoff w h border bordercolor bgcolor =
        let
            val csw = _import "XCreateSimpleWindow" : Word32.word * Word32.word * 
                int * int * int * int * int * Word32.word * Word32.word -> Word32.word ;
        in
            (ref (SOME
                  (case csw (d, win, xoff, yoff, w, h, border, bordercolor, bgcolor) of
                       0w0 => raise X "createsimplewindow failed"
                     | wn => wn)))
        end
      | createsimplewindow _ _ _ _ _ _ _ _ _ = raise X "createsimplewindow: bad display / parent window"


    fun storename (ref (SOME d)) (ref (SOME win)) name =
        let
            val sn = _import "XStoreName" : Word32.word * Word32.word * string -> unit ;
        in
            sn (d, win, name ^ implode([chr(0)]))
        end
      | storename _ _ _ = raise X "storename: bad display / window"


    fun flush (ref (SOME d)) =
        let
            val f = _import "XFlush" : Word32.word -> unit ;
        in
            f d
        end
      | flush _ = raise X "flush: bad display"

    fun mapwindow (ref (SOME d)) (ref (SOME w)) =
        let
            val mw = _import "XMapWindow" : Word32.word * Word32.word -> unit ;
        in
            mw (d, w)
        end
      | mapwindow _ _ = raise X "mapwindow: bad display / window"

    fun raisewindow (ref (SOME d)) (ref (SOME w)) =
        let
            val rw = _import "XRaiseWindow" : Word32.word * Word32.word -> unit ;
        in
            rw (d, w)
        end
      | raisewindow _ _ = raise X "unimplemented"

    fun freegc (ref (SOME d)) (r as (ref (SOME g))) =
        let
            val fg = _import "XFreeGC" : Word32.word * Word32.word -> unit ;
        in
            fg (d, g);
            r := NONE
        end
      | freegc _ _ = raise X "freegc: bad display / gc"

    fun drawtext (ref (SOME d)) (ref (SOME w)) (ref (SOME g)) f x y s =
        let
            val dtn = _import "ml_drawtext_nofont" : Word32.word * Word32.word * Word32.word * 
                                                    int * int * string * int -> unit ;
        in
            case f of
                NONE => dtn (d, w, g, x, y, s, size s)
              | SOME ff => raise X "drawtext: how did you get a font??"
        end
      | drawtext _ _ _ _ _ _ _ = raise X "drawtext: bad display / window / gc"

    fun drawpoint (ref (SOME d)) (ref (SOME w)) (ref (SOME g)) x y =
        let
            val dp = _import "XDrawPoint" : Word32.word * Word32.word * Word32.word * int * int -> unit ;
        in
            dp (d, w, g, x, y)
        end
      | drawpoint _ _ _ _ _ = raise X "drawpoint: bad display / window / gc"

    fun fillrectangle (ref (SOME d)) (ref (SOME w)) (ref (SOME g)) x y wd ht =
        let
            val fr = _import "XFillRectangle" : Word32.word * Word32.word * Word32.word * 
                int * int * int * int -> unit ;
        in
            fr (d, w, g, x, y, wd, ht)
        end
      | fillrectangle _ _ _ _ _ _ _ = raise X "fillrectangle: bad display / window / gc"

    fun drawrectangle (ref (SOME d)) (ref (SOME w)) (ref (SOME g)) x y wd ht =
        let
            val dr = _import "XDrawRectangle" : Word32.word * Word32.word * Word32.word * 
                int * int * int * int -> unit ;
        in
            dr (d, w, g, x, y, wd, ht)
        end
      | drawrectangle _ _ _ _ _ _ _ = raise X "drawrectangle: bad display / window / gc"

    fun drawline (ref (SOME d)) (ref (SOME w)) (ref (SOME g)) x y wd ht =
        let
            val dl = _import "XDrawLine" : Word32.word * Word32.word * Word32.word * 
                int * int * int * int -> unit ;
        in
            dl (d, w, g, x, y, wd, ht)
        end
      | drawline _ _ _ _ _ _ _ = raise X "drawline: bad display / window / gc"

    fun setforeground (ref (SOME d)) (ref (SOME g)) color =
        let
            val sf = _import "XSetForeground" : Word32.word * Word32.word * Word32.word -> unit ;
        in
            sf (d, g, color)
        end
      | setforeground _ _ _ = raise X "setforeground: bad display / gc"

    fun retevent (d, xe, typ) =
        let
            val free = _import "free" : Word32.word -> unit ;

            val f = Vector.sub (handlers, typ)

            (* XXX bye-bye *)
(*          val _ = print ("got type " ^ Int.toString typ ^ "\n") *)

            (* get standard stuff *)
            val st = _import "ml_eventstandard" : Word32.word * int ref * bool ref * Word32.word ref -> unit ;
            val serial = ref 0
            val sendevent = ref false
            val display = ref 0w0
        in      
            st (!xe, serial, sendevent, display);
            (!serial, !sendevent, ref (SOME (!display)), f (!xe)) before
            free (!xe)
        end

    fun nextevent (ref (SOME d)) =
        let
            val ne = _import "ml_nextevent" : Word32.word * Word32.word ref -> int ;
            val xe = ref 0w0
            val typ = ne (d, xe)
        in
            retevent (d, xe, typ)
        end
      | nextevent _ = raise X "nextevent: bad display"

    fun maskevent (ref (SOME d)) m =
        let
            val me = _import "ml_maskevent" : Word32.word * Word32.word * Word32.word ref -> int ;
            val xe = ref 0w0
            val typ = me (d, m, xe)
        in
            retevent (d, xe, typ)
        end
      | maskevent _ _ = raise X "maskevent: bad display"

    fun checkmaskevent (ref (SOME d)) mask =
        let
                                              (* display        mask         event ptr        typ        found? *)
            val cwe = _import "ml_checkmaskevent" : Word32.word * Word32.word * Word32.word ref * int ref -> bool ;
            val xe = ref 0w0
            val typ = ref 0
        in
            case cwe (d, mask, xe, typ) of
                false => NONE
              | true => SOME (retevent (d, xe, !typ))
        end
      | checkmaskevent _ _ = raise X "checkmaskevent: bad display"

    fun getrgb c =
        (Real.fromInt(Word32.toInt(Word32.andb(Word32.>>(c, 0w16), 0wxFF)))/255.0,
         Real.fromInt(Word32.toInt(Word32.andb(Word32.>>(c, 0w8), 0wxFF)))/255.0,
         Real.fromInt(Word32.toInt(Word32.andb(c, 0wxFF)))/255.0)

    fun fromrgb red green blue =
        Word32.<<(Word32.fromInt (Real.trunc(red   * 255.0)), 0w16) +
        Word32.<<(Word32.fromInt (Real.trunc(green * 255.0)), 0w8) +
        Word32.fromInt (Real.trunc(blue * 255.0))

    fun fromints red green blue =
        Word32.<<(Word32.fromInt red, 0w16) +
        Word32.<<(Word32.fromInt green, 0w8) +
        Word32.fromInt blue

    fun mixcolors nil = 0w0 : Word32.word (* XXX ... exception? *)
      | mixcolors l =
        let
            val factor = 1.0 / (Real.fromInt (length l))
            val (r, g, b) = foldl (fn (i, (r,g,b)) =>
                                   case getrgb i of
                                       (rr, gg, bb) => (r + factor * rr,
                                                        g + factor * gg,
                                                        b + factor * bb)) (0.0, 0.0, 0.0) l
        in
            fromrgb r g b
        end

    fun createpixmap (ref (SOME d)) (ref (SOME r)) w h p =
        let
            val cpm = _import "XCreatePixmap" : Word32.word * Word32.word * int * int * int -> Word32.word ;
        in
            case cpm (d, r, w, h, p) of
                0w0 => raise X "createpixmap: failed!"
              | x => (ref (SOME x))
        end
      | createpixmap _ _ _ _ _ = raise X "createpixmap: bad display or drawable"

    (* copyarea d src dest gc srcx srcy width height destx desty *)
    fun copyarea (ref (SOME d)) (ref (SOME src)) (ref (SOME dest)) (ref (SOME gc)) 
             srcx srcy width height destx desty = 
        let
            val cpa = _import "XCopyArea" : Word32.word * Word32.word * Word32.word * Word32.word *
                                          int * int * int * int * int * int -> unit ;
        in
            cpa (d, src, dest, gc, srcx, srcy, width, height, destx, desty)
        end
      | copyarea _ _ _ _ _ _ _ _ _ _ = raise X "copyarea: bad display, src/dest, or gc"

    fun freepixmap (ref (SOME d)) (ref (SOME p)) =
        let
            val fpm = _import "XFreePixmap" : Word32.word * Word32.word -> unit ;
        in
            fpm (d, p)
        end
      | freepixmap _ _ = raise X "freepixmap: bad display or pixmap already freed!"

    (* XXX - probably belongs elsewhere *)
    fun usleep n =
        let
            val us = _import "usleep" : int -> unit ;
        in
            us n
        end

end