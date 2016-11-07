

structure MLX :> MLX =
struct

    type pointer = MLton.Pointer.t
    val null = MLton.Pointer.null
    fun isnull p =
        case MLton.Pointer.compare (p, null) of
            EQUAL => true
          | _ => false

    (* these are all represented as pointers in C *)
    type display = pointer option ref
    type window = pointer option ref
    type gc = pointer option ref

    fun init p = ref (SOME (!p))

    val keychar = ref false

    type pixmap = pointer option ref

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

    datatype modifier = Shift | NumLock | Ctrl |
                        Mod1 | Mod2 | Mod3 | Mod4 | Mod5 |
                        ButtonM of button

    fun maskofmod (m: modifier) =
        let val (b1g, _) = _symbol "MLX_Button1Mask" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val b1 = b1g ()
            val (b2g, _) = _symbol "MLX_Button2Mask" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val b2 = b2g ()
            val (b3g, _) = _symbol "MLX_Button3Mask" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val b3 = b3g ()
            val (b4g, _) = _symbol "MLX_Button4Mask" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val b4 = b4g ()
            val (b5g, _) = _symbol "MLX_Button5Mask" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val b5 = b5g ()
            val (sg, _) = _symbol "MLX_ShiftMask" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val s = sg ()
            val (cg, _) = _symbol "MLX_ControlMask" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val c = cg ()
            val (lg, _) = _symbol "MLX_LockMask" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val l = lg ()
            val (m1g, _) = _symbol "MLX_Mod1Mask" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val m1 = m1g ()
            val (m2g, _) = _symbol "MLX_Mod2Mask" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val m2 = m2g ()
            val (m3g, _) = _symbol "MLX_Mod3Mask" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val m3 = m3g ()
            val (m4g, _) = _symbol "MLX_Mod4Mask" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val m4 = m4g ()
            val (m5g, _) = _symbol "MLX_Mod5Mask" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val m5 = m5g ()
        in
            case m of
                Shift => s
              | NumLock => l
              | Ctrl => c
              | Mod1 => m1
              | Mod2 => m2
              | Mod3 => m3
              | Mod4 => m4
              | Mod5 => m5
              | ButtonM (Button1) => b1
              | ButtonM (Button2) => b2
              | ButtonM (Button3) => b3
              | ButtonM (Button4) => b4
              | ButtonM (Button5) => b5
        end

    fun mods (ms: modstate) =
        let fun is_pressed m =
                Word32.> (Word32.andb (ms, maskofmod m), Word32.fromInt 0)
        in
            List.filter is_pressed
                        [Shift, NumLock, Ctrl, Mod1, Mod2, Mod3, Mod4, Mod5,
                         ButtonM Button1, ButtonM Button2, ButtonM Button3,
                         ButtonM Button4, ButtonM Button5]
        end

    datatype event =
        Any of unit
        (* up/down, window, root, subwindow, time, ptrx, ptry, ptrrx, ptrry, state, keycode, same_screen *)
      | Key of bool * window * window * window * time * int * int * int * int * modstate * Word32.word * bool
        (* up/down, window, root, subwindow, time, ptrx, ptry, ptrrx, ptrry, state, key, same_screen *)
      | KeyC of bool * window * window * window * time * int * int * int * int * modstate * char * bool
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
            val gkp = _import "ml_getmotion" : pointer *
                pointer ref * pointer ref * pointer ref *
                Word32.word ref *
                int ref * int ref *
                int ref * int ref *
                Word32.word ref *
                bool ref *
                bool ref -> unit ;

            val wnd = ref null
            val root = ref null
            val sub = ref null
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
            Motion (init wnd, init root, init sub,
                    !t, !x, !y, !rx, !ry, !state, !ih, !ss)
        end

    fun enternotify _ = raise X "unimplemented: enternotify"
    fun leavenotify _ = raise X "unimplemented: leavenotify"
    fun focusin _ = raise X "unimplemented: focusin"
    fun focusout _ = raise X "unimplemented: focusout"
    fun keymapnotify _ = raise X "unimplemented: keymapnotify"
    fun expose xe =
        let
            val ge = _import "ml_getexpose" : pointer * pointer ref *
                int ref * int ref * int ref * int ref * int ref -> unit ;
            val wnd = ref null
            val x = ref 0
            val y = ref 0
            val w = ref 0
            val h = ref 0
            val c = ref 0
        in
            ge(xe, wnd, x, y, w, h, c);
            Expose (init wnd, !x, !y, !w, !h, !c)
        end

    fun graphicsexpose _ = raise X "unimplemented: graphicsexpose"
    fun noexpose xe =
        let
            val gne = _import "ml_getnoexpose" : pointer * pointer ref * int ref * int ref -> unit ;
            val drw = ref null
            val x   = ref 0
            val y   = ref 0
        in
            gne (xe, drw, x, y);
            NoExpose (init drw, !x, !y)
        end

    fun visibilitynotify _ = raise X "unimplemented: visibilitynotify"
    fun createnotify _ = raise X "unimplemented: createnotify"
    fun destroynotify _ = raise X "unimplemented: destroynotify"
    fun unmapnotify _ = raise X "unimplemented: unmapnotify"

    fun mapnotify xe =
        let
            val gmn = _import "ml_getmapnotify" : pointer * pointer ref * pointer ref * bool ref -> unit ;
            val evt = ref null
            val wnd = ref null
            val orr = ref false
        in
            gmn (xe, evt, wnd, orr);
            Map (init evt, init wnd, !orr)
        end

    fun maprequest xe =
        let
            val gmr = _import "ml_getmaprequest" : pointer * pointer ref * pointer ref -> unit ;
            val par = ref null
            val wnd = ref null
        in
            gmr (xe, par, wnd);
            MapRequest (init par, init wnd)
        end

    fun usechar b = keychar := b
    fun get_usechar () = !keychar

    fun keyevent xe =
        let
            val gkp = _import "ml_getkeypress" : pointer *
                bool ref *
                pointer ref * pointer ref * pointer ref *
                Word32.word ref *
                int ref * int ref *
                int ref * int ref *
                Word32.word ref *
                Word32.word ref *
                bool ref -> unit ;
            val ls = _import "XLookupString" : pointer * char array *
                                               int * pointer * pointer -> int ;
            val isdown = ref false
            val wnd = ref null
            val root = ref null
            val sub = ref null
            val t = ref 0w0
            val x = ref 0
            val y = ref 0
            val rx = ref 0
            val ry = ref 0
            val state = ref 0w0
            val kc = ref 0w0
            val ss = ref false
            val _ = gkp (xe, isdown, wnd, root, sub, t, x, y, rx, ry, state, kc, ss)
        in
            if !keychar then
                let val arr = Array.array (256, #" ")
                    val nc = ls (xe, arr, 256, null, null)
                    fun arr_to_list i =
                        if i >= nc then []
                        else (Array.sub (arr, i))::(arr_to_list (i + 1))
                    val s = String.implode (arr_to_list 0)
                in
                    case Char.fromString s of
                        SOME c =>
                        KeyC (!isdown, init wnd, init root, init sub,
                              !t, !x, !y, !rx, !ry, !state, c, !ss)
                      | NONE =>
                        Key (!isdown, init wnd, init root, init sub,
                             !t, !x, !y, !rx, !ry, !state, !kc, !ss)
                end
            else
                Key (!isdown, init wnd, init root, init sub,
                     !t, !x, !y, !rx, !ry, !state, !kc, !ss)
        end

    fun buttonevent xe =
        let
            val gb = _import "ml_getbutton" : pointer *
                bool ref *
                pointer ref * pointer ref * pointer ref *
                Word32.word ref *
                int ref * int ref *
                int ref * int ref *
                Word32.word ref *
                Word32.word ref *
                bool ref -> unit ;
            val isdown = ref false
            val wnd = ref null
            val root = ref null
            val sub = ref null
            val t = ref 0w0
            val x = ref 0
            val y = ref 0
            val rx = ref 0
            val ry = ref 0
            val state = ref 0w0
            val kc = ref 0w0
            val ss = ref false

            val (b1g, _) = _symbol "MLX_Button1" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val b1 = b1g ()
            val (b2g, _) = _symbol "MLX_Button2" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val b2 = b2g ()
            val (b3g, _) = _symbol "MLX_Button3" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val b3 = b3g ()
            val (b4g, _) = _symbol "MLX_Button4" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val b4 = b4g ()
            val (b5g, _) = _symbol "MLX_Button5" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
            val b5 = b5g ()
        in
            gb (xe, isdown, wnd, root, sub, t, x, y, rx, ry, state, kc, ss);
            Button (!isdown, init wnd, init root, init sub,
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
            val gcn = _import "ml_getconfigurenotify" : pointer *
                pointer ref *
                pointer ref *
                int ref * int ref * int ref * int ref *
                int ref *
                pointer ref * bool ref -> unit ;

            val evt = ref null
            val wnd = ref null
            val x = ref 0
            val y = ref 0
            val w = ref 0
            val h = ref 0
            val b = ref 0
            val abo = ref null
            val orr = ref false
        in
            gcn (xe, evt, wnd, x, y, w, h, b, abo, orr);
            Configure (init evt, init wnd, !x, !y, !w, !h, !b, init abo, !orr)
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

        val (noeventg, _) = _symbol "MLX_NoEventMask" :
                      (unit -> Word32.word) * (Word32.word -> unit) ;
        val noevent = noeventg ()
        val (keypressg, _) = _symbol "MLX_KeyPressMask" :
                       (unit -> Word32.word) * (Word32.word -> unit) ;
        val keypress = keypressg ()
        val (keyreleaseg, _) = _symbol "MLX_KeyReleaseMask" :
                         (unit -> Word32.word) * (Word32.word -> unit) ;
        val keyrelease = keyreleaseg ()
        val (buttonpressg, _) = _symbol "MLX_ButtonPressMask" :
                          (unit -> Word32.word) * (Word32.word -> unit) ;
        val buttonpress = buttonpressg ()
        val (buttonreleaseg, _) = _symbol "MLX_ButtonReleaseMask" :
                            (unit -> Word32.word) * (Word32.word -> unit) ;
        val buttonrelease = buttonreleaseg ()
        val (enterwindowg, _) = _symbol "MLX_EnterWindowMask" :
                          (unit -> Word32.word) * (Word32.word -> unit) ;
        val enterwindow = enterwindowg ()
        val (leavewindowg, _) = _symbol "MLX_LeaveWindowMask" :
                          (unit -> Word32.word) * (Word32.word -> unit) ;
        val leavewindow = leavewindowg ()
        val (pointermotiong, _) = _symbol "MLX_PointerMotionMask" :
                            (unit -> Word32.word) * (Word32.word -> unit) ;
        val pointermotion = pointermotiong ()
        val (pointermotionhintg, _) = _symbol "MLX_PointerMotionHintMask" :
                                (unit -> Word32.word) * (Word32.word -> unit) ;
        val pointermotionhint = pointermotionhintg ()
        val (button1motiong, _) = _symbol "MLX_Button1MotionMask" :
                            (unit -> Word32.word) * (Word32.word -> unit) ;
        val button1motion = button1motiong ()
        val (button2motiong, _) = _symbol "MLX_Button2MotionMask" :
                            (unit -> Word32.word) * (Word32.word -> unit) ;
        val button2motion = button2motiong ()
        val (button3motiong, _) = _symbol "MLX_Button3MotionMask" :
                            (unit -> Word32.word) * (Word32.word -> unit) ;
        val button3motion = button3motiong ()
        val (button4motiong, _) = _symbol "MLX_Button4MotionMask" :
                            (unit -> Word32.word) * (Word32.word -> unit) ;
        val button4motion = button3motiong ()
        val (button5motiong, _) = _symbol "MLX_Button5MotionMask" :
                            (unit -> Word32.word) * (Word32.word -> unit) ;
        val button5motion = button3motiong ()
        val (buttonmotiong, _) = _symbol "MLX_ButtonMotionMask" :
                           (unit -> Word32.word) * (Word32.word -> unit) ;
        val buttonmotion = button3motiong ()
        val (keymapstateg, _) = _symbol "MLX_KeymapStateMask" :
                          (unit -> Word32.word) * (Word32.word -> unit) ;
        val keymapstate = keymapstateg ()
        val (exposureg, _) = _symbol "MLX_ExposureMask" :
                       (unit -> Word32.word) * (Word32.word -> unit) ;
        val exposure = exposureg ()
        val (visibilitychangeg, _) = _symbol "MLX_VisibilityChangeMask" :
                               (unit -> Word32.word) * (Word32.word -> unit) ;
        val visibilitychange = visibilitychangeg ()
        val (structurenotifyg, _) = _symbol "MLX_StructureNotifyMask" :
                              (unit -> Word32.word) * (Word32.word -> unit) ;
        val structurenotify = structurenotifyg ()
        val (resizeredirectg, _) = _symbol "MLX_ResizeRedirectMask" :
                             (unit -> Word32.word) * (Word32.word -> unit) ;
        val resizeredirect = resizeredirectg ()
        val (substructurenotifyg, _) = _symbol "MLX_SubstructureNotifyMask" :
                                 (unit -> Word32.word) * (Word32.word -> unit) ;
        val substructurenotify = substructurenotifyg ()
        val (substructureredirectg, _) = _symbol "MLX_SubstructureRedirectMask" :
                                   (unit -> Word32.word) * (Word32.word -> unit) ;
        val substructureredirect = substructureredirectg ()
        val (focuschangeg, _) = _symbol "MLX_FocusChangeMask" :
                          (unit -> Word32.word) * (Word32.word -> unit) ;
        val focuschange = focuschangeg ()
        val (propertychangeg, _) = _symbol "MLX_PropertyChangeMask" :
                             (unit -> Word32.word) * (Word32.word -> unit) ;
        val propertychange = propertychangeg ()
        val (colormapchangeg, _) = _symbol "MLX_ColormapChangeMask" :
                             (unit -> Word32.word) * (Word32.word -> unit) ;
        val colormapchange = colormapchangeg ()
        val (ownergrabbuttong, _) = _symbol "MLX_OwnerGrabButtonMask" :
                              (unit -> Word32.word) * (Word32.word -> unit) ;
        val ownergrabbutton = ownergrabbuttong ()
        val empty = 0w0 : Word32.word
        fun make l = foldl Word32.orb empty l
        fun contains m i = Word32.> (Word32.andb (m, i), 0w0)
        fun set m i = Word32.orb (m, i)
        fun unset m i = Word32.andb(m, Word32.notb i)
    end

    fun selectinput (ref (SOME d)) (ref (SOME w)) m =
        let
            val si = _import "XSelectInput" : pointer * pointer * Word32.word -> unit ;
        in
            si (d, w, m)
        end
      | selectinput _ _ _ = raise X "selectinput: bad display/window"

    fun opendisplay so  : display =
        let
            (* hehe. I want to pass either 0 ("NULL") or the string,
               so this function can have both types.. *)
            val od  = _import "XOpenDisplay" : string -> pointer ;
            val od0 = _import "XOpenDisplay" : int -> pointer ;
            val r = case so of
                        NONE => od0 0
                      | SOME s => (od (s ^ implode([chr 0])))
        in
            if isnull r then raise X "opendisplay failed"
            else ref (SOME r)
        end

    fun closedisplay ((ref NONE) : display) = raise X "display already closed"
      | closedisplay (r as (ref (SOME d))) : unit =
        let
            (* XXX check return value? *)
            val cd = _import "XCloseDisplay" : pointer -> unit ;
        in
            cd d;
            r := NONE
        end

    fun initthreads () =
       let val f =  _import "XInitThreads" : unit -> int;
       in
           if f () = 0 then
               raise X "initthreads failed"
           else
               ()
       end


    fun creategc (ref (SOME d)) (ref (SOME w)) : gc =
        let
            val cg = _import "ml_creategc" : pointer * pointer -> pointer ;
            val gc = cg (d, w)
        in
            if isnull gc then raise X "creategc failed"
            else ref (SOME gc)
        end
      | creategc (_ : display) (_ : window) = raise X "creategc: bad display / window"


    fun defaultrootwindow (ref (SOME d))  : window =
        let
            val drw = _import "XDefaultRootWindow" : pointer -> pointer ;
            val w = drw d
        in
            if isnull w then raise X "defaultrootwindow failed"
            else ref (SOME w)
        end
      | defaultrootwindow (_ : display) = raise X "defaultrootwindow: bad display"

    fun createsimplewindow (ref (SOME d)) (ref (SOME win)) xoff yoff w h border bordercolor bgcolor =
        let
            val csw = _import "XCreateSimpleWindow" : pointer * pointer *
                int * int * int * int * int * Word32.word * Word32.word -> pointer ;
            val w = csw (d, win, xoff, yoff, w, h, border, bordercolor, bgcolor)
        in
            if isnull w then raise X "createsimplewindow failed"
            else ref (SOME w)
        end
      | createsimplewindow _ _ _ _ _ _ _ _ _ = raise X "createsimplewindow: bad display / parent window"


    fun storename (ref (SOME d)) (ref (SOME win)) name =
        let
            val sn = _import "XStoreName" : pointer * pointer * string -> unit ;
        in
            sn (d, win, name ^ implode([chr(0)]))
        end
      | storename _ _ _ = raise X "storename: bad display / window"


    fun flush (ref (SOME d)) =
        let
            val f = _import "XFlush" : pointer -> unit ;
        in
            f d
        end
      | flush _ = raise X "flush: bad display"

    fun mapwindow (ref (SOME d)) (ref (SOME w)) =
        let
            val mw = _import "XMapWindow" : pointer * pointer -> unit ;
        in
            mw (d, w)
        end
      | mapwindow _ _ = raise X "mapwindow: bad display / window"

    fun raisewindow (ref (SOME d)) (ref (SOME w)) =
        let
            val rw = _import "XRaiseWindow" : pointer * pointer -> unit ;
        in
            rw (d, w)
        end
      | raisewindow _ _ = raise X "unimplemented"

    fun freegc (ref (SOME d)) (r as (ref (SOME g))) =
        let
            val fg = _import "XFreeGC" : pointer * pointer -> unit ;
        in
            fg (d, g);
            r := NONE
        end
      | freegc _ _ = raise X "freegc: bad display / gc"

    fun drawtext (ref (SOME d)) (ref (SOME w)) (ref (SOME g)) f x y s =
        let
            val dtn = _import "ml_drawtext_nofont" : pointer * pointer * pointer *
                                                    int * int * string * int -> unit ;
        in
            case f of
                NONE => dtn (d, w, g, x, y, s, size s)
              | SOME ff => raise X "drawtext: how did you get a font??"
        end
      | drawtext _ _ _ _ _ _ _ = raise X "drawtext: bad display / window / gc"

    fun drawpoint (ref (SOME d)) (ref (SOME w)) (ref (SOME g)) x y =
        let
            val dp = _import "XDrawPoint" : pointer * pointer * pointer * int * int -> unit ;
        in
            dp (d, w, g, x, y)
        end
      | drawpoint _ _ _ _ _ = raise X "drawpoint: bad display / window / gc"

    fun fillrectangle (ref (SOME d)) (ref (SOME w)) (ref (SOME g)) x y wd ht =
        let
            val fr = _import "XFillRectangle" : pointer * pointer * pointer *
                int * int * int * int -> unit ;
        in
            fr (d, w, g, x, y, wd, ht)
        end
      | fillrectangle _ _ _ _ _ _ _ = raise X "fillrectangle: bad display / window / gc"

    fun drawrectangle (ref (SOME d)) (ref (SOME w)) (ref (SOME g)) x y wd ht =
        let
            val dr = _import "XDrawRectangle" : pointer * pointer * pointer *
                int * int * int * int -> unit ;
        in
            dr (d, w, g, x, y, wd, ht)
        end
      | drawrectangle _ _ _ _ _ _ _ = raise X "drawrectangle: bad display / window / gc"

    fun drawline (ref (SOME d)) (ref (SOME w)) (ref (SOME g)) x y wd ht =
        let
            val dl = _import "XDrawLine" : pointer * pointer * pointer *
                int * int * int * int -> unit ;
        in
            dl (d, w, g, x, y, wd, ht)
        end
      | drawline _ _ _ _ _ _ _ = raise X "drawline: bad display / window / gc"

    fun drawrgbarray (ref (SOME d)) (ref (SOME w)) (ref (SOME g)) x y wd ht arr =
        let
            val da = _import "drawrgbarray" : pointer * pointer * pointer *
                int * int * int * int * char Array.array -> unit ;
        in
            da (d, w, g, x, y, wd, ht, arr)
        end
      | drawrgbarray _ _ _ _ _ _ _ _ = raise X "drawrgbarray: bad display / window / gc"


    fun setforeground (ref (SOME d)) (ref (SOME g)) color =
        let
            val sf = _import "XSetForeground" : pointer * pointer * Word32.word -> unit ;
        in
            sf (d, g, color)
        end
      | setforeground _ _ _ = raise X "setforeground: bad display / gc"

    fun retevent (d, xe, typ) =
        let
            val free = _import "free" : pointer -> unit ;

            val f = Vector.sub (handlers, typ)

            (* XXX bye-bye *)
(*          val _ = print ("got type " ^ Int.toString typ ^ "\n") *)

            (* get standard stuff *)
            val st = _import "ml_eventstandard" : pointer * int ref * bool ref * pointer ref -> unit ;
            val serial = ref 0
            val sendevent = ref false
            val display = ref null
        in
            st (!xe, serial, sendevent, display);
            (!serial, !sendevent, init display, f (!xe)) before
            free (!xe)
        end

    fun nextevent (ref (SOME d)) =
        let
            val ne = _import "ml_nextevent" : pointer * pointer ref -> int ;
            val xe = ref null
            val typ = ne (d, xe)
        in
            retevent (d, xe, typ)
        end
      | nextevent _ = raise X "nextevent: bad display"

    fun maskevent (ref (SOME d)) m =
        let
            val me = _import "ml_maskevent" : pointer * Word32.word * pointer ref -> int ;
            val xe = ref null
            val typ = me (d, m, xe)
        in
            retevent (d, xe, typ)
        end
      | maskevent _ _ = raise X "maskevent: bad display"

    fun checkmaskevent (ref (SOME d)) mask =
        let
                                              (* display        mask         event ptr        typ        found? *)
            val cwe = _import "ml_checkmaskevent" : pointer * Word32.word * pointer ref * int ref -> bool ;
            val xe = ref null
            val typ = ref 0
        in
            case cwe (d, mask, xe, typ) of
                false => NONE
              | true => SOME (retevent (d, xe, !typ))
        end
      | checkmaskevent _ _ = raise X "checkmaskevent: bad display"

    (* display  window  root  subwindow  root_x  root_y
       win_x  win_y  button_modifiers *)
    val qp = _import "XQueryPointer" : pointer * pointer * pointer ref * pointer ref * int ref * int ref * int ref * int ref * Word32.word ref -> bool;

    fun mousepos (ref (SOME d)) (ref (SOME w)) =
        let
            val root = ref null
            val sw = ref null
            val rootx = ref 0
            val rooty = ref 0
            val winx = ref 0
            val winy = ref 0
            val btns = ref (Word32.fromInt 0)

        in
            if qp (d, w, root, sw, rootx, rooty, winx, winy, btns) then
                (!winx, !winy)
            else
                (~1, ~1)
        end
      | mousepos _ _ = raise X "mousepos: bad display"

    fun buttons (ref (SOME d)) (ref (SOME w)) =
        let
            val root = ref null
            val sw = ref null
            val rootx = ref 0
            val rooty = ref 0
            val winx = ref 0
            val winy = ref 0
            val btns = ref (Word32.fromInt 0)
            val _ = qp (d, w, root, sw, rootx, rooty, winx, winy, btns)
(*
            val _ = print (Word32.fmt StringCvt.HEX b1)
            val _ = print "\n"
            val _ = print (Word32.fmt StringCvt.HEX (!btns))
            val _ = print "\n"
*)
        in
            List.foldl
                (fn (x, r) =>
                     case x of
                         ButtonM b => b::r
                       | _ => r)
                []
                (mods (!btns))
        end
      | buttons _ _ = raise X "buttons: bad display"

    fun button d w = List.length (buttons d w) > 0

    fun modstate (ref (SOME d)) (ref (SOME w)) =
        let
            val root = ref null
            val sw = ref null
            val rootx = ref 0
            val rooty = ref 0
            val winx = ref 0
            val winy = ref 0
            val ms = ref (Word32.fromInt 0)
            val _ = qp (d, w, root, sw, rootx, rooty, winx, winy, ms)
        in
            mods (!ms)
        end
      | modstate _ _ = raise X "mousepos: bad display"

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
            val cpm = _import "XCreatePixmap" : pointer * pointer * int * int * int -> pointer ;
            val pm = cpm (d, r, w, h, p)
        in
            if isnull pm then raise X "createpixmap: failed!"
            else ref (SOME pm)
        end
      | createpixmap _ _ _ _ _ = raise X "createpixmap: bad display or drawable"

    (* copyarea d src dest gc srcx srcy width height destx desty *)
    fun copyarea (ref (SOME d)) (ref (SOME src)) (ref (SOME dest)) (ref (SOME gc)) 
             srcx srcy width height destx desty = 
        let
            val cpa = _import "XCopyArea" : pointer * pointer * pointer * pointer *
                                          int * int * int * int * int * int -> unit ;
        in
            cpa (d, src, dest, gc, srcx, srcy, width, height, destx, desty)
        end
      | copyarea _ _ _ _ _ _ _ _ _ _ = raise X "copyarea: bad display, src/dest, or gc"

    fun freepixmap (ref (SOME d)) (ref (SOME p)) =
        let
            val fpm = _import "XFreePixmap" : pointer * pointer -> unit ;
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
