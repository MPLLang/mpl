
(* simple interface to C X Lib *)

(* Contributors: Tom Murphy VII, Dave Kitchin, Adam Goode *)

(* gc is a "graphics context", not anything to do with garbage collection. *)

signature MLX =
sig
    
    type display
    type window
    type gc
    type color = Word32.word

    type pixmap

    type drawable

    (* state of keyboard / mouse modifiers *)
    type modstate
    type time
    type serial

    exception X of string

    val opendisplay : string option -> display

    val defaultrootwindow : display -> window

    (* createsimplewindow d w xoff yoff width height borderwidth border background *)
    val createsimplewindow : display -> window -> 
           int -> int -> int -> int -> int -> color -> color -> window

    (* sets the name of a window *)
    val storename : display -> window -> string -> unit

    val mapwindow : display -> window -> unit

    val raisewindow : display -> window -> unit

    val flush : display -> unit

    val closedisplay : display -> unit

    (* drawable coercions *)
    val wd : window -> drawable
    val pd : pixmap -> drawable

    (* operations on GCs *)

    (* doesn't let you specify default values. Just set them afterwards *)
    val creategc : display -> drawable -> gc
        
    val freegc : display -> gc -> unit

    val setforeground : display -> gc -> color -> unit

    datatype button =
        Button1 | Button2 | Button3 | Button4 | Button5

    (* events *)
    datatype event = 
        Any of unit
        (* up/down, window, root, subwindow, time, ptrx, ptry, ptrrx, ptrry, state, keycode, same_screen *)
      | Key of bool * window * window * window * time * int * int * int * int * modstate * Word32.word * bool
        (* window, root, subwindow, time, ptrx, ptry, ptrrx, ptrry, state, ishint, same_screen *)
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

    structure Mask :
    sig
        type item
        type mask
            
        val noevent : item
        val keypress : item
        val keyrelease : item
        val buttonpress : item
        val buttonrelease : item
        val enterwindow : item
        val leavewindow : item
        val pointermotion : item
        val pointermotionhint : item
        val button1motion : item
        val button2motion : item
        val button3motion : item
        val button4motion : item
        val button5motion : item
        val buttonmotion : item
        val keymapstate : item
        val exposure : item
        val visibilitychange : item
        val structurenotify : item
        val resizeredirect : item
        val substructurenotify : item
        val substructureredirect : item
        val focuschange : item
        val propertychange : item
        val colormapchange : item
        val ownergrabbutton : item
            
        val make : item list -> mask
        val contains : mask -> item -> bool
        val set : mask -> item -> mask
        val unset : mask -> item -> mask
        val empty : mask
    end

    (* for example,
       selectinput d w (Mask.make [Mask.PropertyChange, Mask.Exposure])
       *)
    val selectinput : display -> window -> Mask.mask -> unit

    (* Blocks until an event is available, and returns it. *)
    val nextevent : display -> serial * bool * display * event

    (* Blocks until an event matching the mask is available. *)
    val maskevent : display -> Mask.mask -> serial * bool * display * event

    (* Finds the first event matching the mask, if one exists. Returns immediately. *)
    val checkmaskevent : display -> Mask.mask -> (serial * bool * display * event) option

    (* pixmaps *)
    (* createpixmap d dw width height depth *)
    val createpixmap : display -> drawable -> int -> int -> int -> pixmap
    val freepixmap : display -> pixmap -> unit

    (* copyarea d src dest gc srcx srcy width height destx desty *)
    val copyarea : display -> drawable -> drawable -> gc -> int -> int -> int -> int -> int -> int -> unit

    (* colors *)
    val getrgb : color -> real * real * real
    val fromints : int -> int -> int -> color
    val fromrgb : real -> real -> real -> color
    val mixcolors : color list -> color

    (* drawing *)
   
    (* what's this? *)
    type font

    (* drawtext d w g font x y text *)
    val drawtext : display -> drawable -> gc -> font option -> int -> int -> string -> unit
    
    (* drawpoint d w g x y *)
    val drawpoint : display -> drawable -> gc -> int -> int -> unit
    val fillrectangle : display -> drawable -> gc -> int -> int -> int -> int -> unit
    val drawline : display -> drawable -> gc -> int -> int -> int -> int -> unit
    val drawrectangle : display -> drawable -> gc -> int -> int -> int -> int -> unit

    (* usleep microseconds *)
    val usleep : int -> unit

end