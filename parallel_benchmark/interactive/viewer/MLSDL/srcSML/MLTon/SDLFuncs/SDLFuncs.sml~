(* Written by ANOQ of the Sun <anoq@HardcoreProcessing.com>

   There is no warranty of any kind and I will not be
   held responsible for any problems it may cause.

   Use at your own risk! *)

(* This is SDL::ML, an SML interface to some of the
   C functions in Sam Lantiga's SDL library *)


exception SDLMLError

(* This is in the name of type-safety! *)
signature HIDDEN_INT =
  sig
    type t

    val fromInt : int -> t
    val toInt : t -> int
  end

structure SDLSurface :> HIDDEN_INT =
  struct
    type t = int

    fun fromInt a = a : t
    fun toInt a = a : int
  end

structure HcPSound :> HIDDEN_INT =
  struct
    type t = int

    fun fromInt a = a : t
    fun toInt a = a : int
  end

structure SDLEvent :> HIDDEN_INT =
  struct
    type t = int

    fun fromInt a = a : t
    fun toInt a = a : int
  end

type SDL_Surface = SDLSurface.t
type SDL_Event = SDLEvent.t
type HcP_Sound = HcPSound.t

(* Test Imager *)
(*fun testimager(destSurf : SDL_Surface, srcSurf : SDL_Surface,
               destWidth : int, destHeight : int) : unit =
      prim ("@mltestimager", "@mltestimager",
            (SDLSurface.toInt destSurf,
             SDLSurface.toInt srcSurf,
             destWidth, destHeight)) *)
(* End of test imager *)

(* SDL constants that we will need for SDL_Init... *)
(* val SDL_INIT_AUDIO = 0x0010
   val SDL_INIT_VIDEO = 0x0020 *)

(* This calls SDL_Init with SDL_INIT_AUDIO | SDL_INIT_VIDEO *)
val SDL_InitDefault = _import "mltSDL_InitDefault": unit -> int;

val SDL_Quit = _import "mltSDL_Quit": unit -> unit;

(* SDL constants that we will need for SDL_SetVideoMode... *)
(* val SDL_SWSURFACE = 0x00000000
   val SDL_FULLSCREEN = 0x80000000 FIXME: This will not fit in signed int...*)

(* This calls SDL_SetVideoMode with SDL_SWSURFACE | SDL_FULLSCREEN *)
fun SDL_SetVideoModeDefault (width, height, bpp : int) : SDL_Surface =
      let
        val f = _import "mltSDL_SetVideoModeDefault": int * int * int -> int;
      in
        SDLSurface.fromInt
          (f (width, height, bpp))
      end

fun SDL_BlitSurface(srcSurf : SDL_Surface,
                    srcX : int, srcY : int,
                    srcW : int, srcH : int,
                    dstSurf : SDL_Surface,
                    dstX : int, dstY : int,
                    dstW : int, dstH : int) : int =
      let
        val f = _import "mltSDL_BlitSurface": int * int * int * int * int * int * int * int * int * int -> int;
      in
        f (SDLSurface.toInt srcSurf,
           srcX, srcY, srcW, srcH,
           SDLSurface.toInt dstSurf,
           dstX, dstY, dstW, dstH)
      end

fun SDL_UpdateRect(surf : SDL_Surface,
                   x : int, y : int, w : int, h : int) : unit =
      let
        val f = _import "mltSDL_UpdateRect": int * int * int * int * int -> unit;
      in
        f (SDLSurface.toInt surf, x, y, w, h)
      end

fun zeroTerminate str =
      str ^ "\000"

fun SDL_LoadBMP(fileName : string) : SDL_Surface =
      let
        val f = _import "mltSDL_LoadBMP": string -> int;
        val i = f (zeroTerminate fileName)
      in
        if i = 0 then
          raise SDLMLError
        else
          SDLSurface.fromInt i
      end

fun SDL_AllocSurfaceDefaultAlpha(width : int, height : int, depth : int,
                                 Rmask : int, Gmask : int, Bmask : int,
                                 Amask : int) : SDL_Surface =
      let
        val f = _import "mltSDL_AllocSurfaceDefaultAlpha": int * int * int * int * int * int * int -> int;
      in
        SDLSurface.fromInt (f (width, height, depth, Rmask, Gmask, Bmask, Amask))
      end

fun SDL_FreeSurface(surf : SDL_Surface) : unit =
      let
        val f = _import "mltSDL_FreeSurface": int -> unit;
      in
        f (SDLSurface.toInt surf)
      end

val SDL_SetEventFilterDefault =
      _import "mltSDL_SetEventFilterDefault": unit -> unit;

val HcP_GetMouseButton =
      _import "mltHcP_GetMouseButton": unit -> int;

val HcP_GetMouseX =
      _import "mltHcP_GetMouseX": unit -> int;

val HcP_GetMouseY =
      _import "mltHcP_GetMouseY": unit -> int;

fun SDL_GetMouseState () =
      (HcP_GetMouseButton(),
       (HcP_GetMouseX (), HcP_GetMouseY ()))

fun SDL_FillRect(surf : SDL_Surface,
                 x : int, y : int,
                 w : int, h : int, col : int) : int =
      let
        val f = _import "mltSDL_FillRect": int * int * int * int * int * int -> int;
      in
        f (SDLSurface.toInt surf, x, y, w, h, col)
      end

fun SDL_MapRGB(surf : SDL_Surface, r : int, g : int, b : int) : int =
      let
        val f = _import "mltSDL_MapRGB": int * int * int * int -> int;
      in
        f (SDLSurface.toInt surf, r, g, b)
      end

fun SDL_SetColorKeySource(surf : SDL_Surface, col : int) : int =
      let
        val f = _import "mltSDL_SetColorKeySource": int * int -> int;
      in
        f (SDLSurface.toInt surf, col)
      end

fun SDL_SetAlphaSource(surf : SDL_Surface, alpha : int) : int =
      let
        val f = _import "mltSDL_SetAlphaSource": int * int -> int;
      in
        f (SDLSurface.toInt surf, alpha)
      end

fun HcP_PutPixel(surf : SDL_Surface, r : int, g : int, b : int, x : int, y : int) : unit =
      let
        val f = _import "mltHcP_PutPixel": int * int * int * int * int * int -> unit;
      in
        f (SDLSurface.toInt surf, r, g, b, x, y)
      end

fun HcP_GetPixel(surf : SDL_Surface, x : int, y : int) : int =
      let
        val f = _import "mltHcP_GetPixel": int * int * int -> int;
      in
        f (SDLSurface.toInt surf, x, y)
      end

val SDL_GetTicks =
      _import "mltSDL_GetTicks": unit -> int;

val SDL_Delay =
      _import "mltSDL_Delay": int -> unit;

val HcP_ExecuteFile =
      _import "mltHcP_ExecuteFile": string -> unit;

fun HcP_GetImageWidth(surf : SDL_Surface) : int =
      let
        val f = _import "mltHcP_GetImageWidth": int -> int;
      in
        f (SDLSurface.toInt surf)
      end

fun HcP_GetImageHeight(surf : SDL_Surface) : int =
      let
        val f = _import "mltHcP_GetImageHeight": int -> int;
      in
        f (SDLSurface.toInt surf)
      end

(* SDL Eventcodes *)
val SDL_NOEVENT         = 0 (* Used in SDL::ML for yet unimplemented events *)
val SDL_ACTIVEEVENT     = 1 (* Not implemented *)
val SDL_KEYDOWN         = 2
val SDL_KEYUP           = 3
val SDL_MOUSEMOTION     = 4
val SDL_MOUSEBUTTONDOWN = 5
val SDL_MOUSEBUTTONUP   = 6
val SDL_JOYMOTION       = 7 (* Not implemented *)
val SDL_JOYBUTTONDOWN   = 8 (* Not implemented *)
val SDL_JOYBUTTONUP     = 9 (* Not implemented *)
val SDL_QUIT            = 10
val SDL_SYSWMEVENT      = 11 (* Not implemented *)
val SDL_NUMEVENTS       = 12 (* Not implemented *)

(* SDL Keycodes *)
val SDLK_BACKSPACE = 8
val SDLK_TAB = 9
val SDLK_CLEAR = 12
val SDLK_RETURN = 13
val SDLK_ESCAPE = 27
val SDLK_SPACE = 32
val SDLK_QUOTE = 39
val SDLK_COMMA = 44
val SDLK_MINUS = 45
val SDLK_PERIOD = 46
val SDLK_SLASH = 47
val SDLK_0 = 48
val SDLK_1 = 49
val SDLK_2 = 50
val SDLK_3 = 51
val SDLK_4 = 52
val SDLK_5 = 53
val SDLK_6 = 54
val SDLK_7 = 55
val SDLK_8 = 56
val SDLK_9 = 57
val SDLK_SEMICOLON = 59
val SDLK_EQUALS = 61
val SDLK_KP0 = 70
val SDLK_KP1 = 71
val SDLK_KP2 = 72
val SDLK_KP3 = 73
val SDLK_KP4 = 74
val SDLK_KP5 = 75
val SDLK_KP6 = 76
val SDLK_KP7 = 77
val SDLK_KP8 = 78
val SDLK_KP9 = 79
val SDLK_KP_PERIOD = 80
val SDLK_KP_DIVIDE = 81
val SDLK_KP_MULTIPLY = 82
val SDLK_KP_MINUS = 83
val SDLK_KP_PLUS = 84
val SDLK_KP_ENTER = 85
val SDLK_KP_EQUALS = 86
val SDLK_LEFTBRACKET = 91
val SDLK_BACKSLASH = 92
val SDLK_RIGHTBRACKET = 93
val SDLK_BACKQUOTE = 96
val SDLK_a = 97
val SDLK_b = 98
val SDLK_c = 99
val SDLK_d = 100
val SDLK_e = 101
val SDLK_f = 102
val SDLK_g = 103
val SDLK_h = 104
val SDLK_i = 105
val SDLK_j = 106
val SDLK_k = 107
val SDLK_l = 108
val SDLK_m = 109
val SDLK_n = 110
val SDLK_o = 111
val SDLK_p = 112
val SDLK_q = 113
val SDLK_r = 114
val SDLK_s = 115
val SDLK_t = 116
val SDLK_u = 117
val SDLK_v = 118
val SDLK_w = 119
val SDLK_x = 120
val SDLK_y = 121
val SDLK_z = 122
val SDLK_DELETE = 127
val SDLK_F1 = 128
val SDLK_F2 = 129
val SDLK_F3 = 130
val SDLK_F4 = 131
val SDLK_F5 = 132
val SDLK_F6 = 133
val SDLK_F7 = 134
val SDLK_F8 = 135
val SDLK_F9 = 136
val SDLK_F10 = 137
val SDLK_F11 = 138
val SDLK_F12 = 139
val SDLK_F13 = 140
val SDLK_F14 = 141
val SDLK_F15 = 142
val SDLK_PAUSE = 143
val SDLK_NUMLOCK = 144
val SDLK_UP = 145
val SDLK_DOWN = 146
val SDLK_RIGHT = 147
val SDLK_LEFT = 148
val SDLK_INSERT = 149
val SDLK_HOME = 150
val SDLK_END = 151
val SDLK_PAGEUP = 152
val SDLK_PAGEDOWN = 153
val SDLK_CAPSLOCK = 154
val SDLK_SCROLLOCK = 155
val SDLK_RSHIFT = 156
val SDLK_LSHIFT = 157
val SDLK_RCTRL = 158
val SDLK_LCTRL = 159
val SDLK_RALT = 160
val SDLK_LALT = 161
val SDLK_RMETA = 162
val SDLK_LMETA = 163
val SDLK_HELP = 164
val SDLK_PRINT = 165
val SDLK_SYSREQ = 166
val SDLK_MENU = 167
val SDLK_BREAK = 168
val SDLK_EURO = 169		(* Some european keyboards *)
val SDLK_POWER = 170		(* Power Macintosh power key *)
(* Add any other keys (to a maximum of 255) here *)
val SDLK_NONE = 255

fun SDL_SetPrinterDocTitle (title : string) : unit =
      let
        val f = _import "mltSDL_SetPrinterDocTitle": string -> unit;
      in
        f (zeroTerminate title)
      end

val SDL_GetPageWidth =
      _import "mltSDL_GetPageWidth": unit -> int;

val SDL_GetPageHeight =
      _import "mltSDL_GetPageHeight": unit -> int;

val SDL_AbortDoc =
      _import "mltSDL_AbortDoc": unit -> unit;

fun SDL_BeginDoc () : SDL_Surface =
      let
        val f = _import "mltSDL_BeginDoc": unit -> int;
      in
        SDLSurface.fromInt (f ())
      end

val SDL_EndDoc =
      _import "mltSDL_EndDoc": unit -> unit;

fun SDL_NewPage () : SDL_Surface =
      let
        val f = _import "mltSDL_NewPage": unit -> int;
      in
        SDLSurface.fromInt (f ())
      end

fun SDL_WaitEvent () : SDL_Event =
      let
        val f = _import "mltSDL_WaitEvent": unit -> int;
      in
        SDLEvent.fromInt (f ())
      end

fun SDL_PollEvent () : SDL_Event =
      let
        val f = _import "mltSDL_PollEvent": unit -> int;
      in
        SDLEvent.fromInt (f ())
      end

fun HcP_EventCode (e : SDL_Event) : int =
      let
        val f = _import "mltHcP_EventCode": int -> int;
      in
        f (SDLEvent.toInt e)
      end

fun HcP_EventValue (e : SDL_Event) : int =
      let
        val f = _import "mltHcP_EventValue": int -> int;
      in
        f (SDLEvent.toInt e)
      end

fun HcP_WaitEvent () =
      let
        val ev = SDL_WaitEvent ()
        val code = HcP_EventCode ev
        val value = HcP_EventValue ev
      in
        (code, value)
      end

fun HcP_PollEvent () =
      let
        val ev = SDL_PollEvent ()
        val code = HcP_EventCode ev
        val value = HcP_EventValue ev
      in
        (code, value)
      end

val HcP_DanishAE =
      _import "mltHcP_DanishAE": unit -> int;

val HcP_DanishOE =
      _import "mltHcP_DanishOE": unit -> int;

val HcP_DanishAA =
      _import "mltHcP_DanishAA": unit -> int;

(* The rest is the Hardcore Processing Audio System (TM) :) *)
(* It will just play multiple sounds at the same time - nothing fancy. *)

val HcP_AudioBegin =
      _import "mltHcP_AudioBegin": unit -> int;

val HcP_AudioEnd =
      _import "mltHcP_AudioEnd": unit -> unit;

fun HcP_SoundFree(sound : HcP_Sound) : unit =
      let
        val f = _import "mltHcP_SoundFree": int -> unit;
      in
        f (HcPSound.toInt sound)
      end

fun HcP_LoadWAV(fileName : string) : HcP_Sound =
      let
        val f = _import "mltHcP_LoadWAV": string -> int;
      in
        HcPSound.fromInt (f (zeroTerminate fileName))
      end

fun HcP_SoundSetRate(sound : HcP_Sound, freq : int) : unit =
      let
        val f = _import "mltHcP_SoundSetRate": int * int -> unit;
      in
        f (HcPSound.toInt sound, freq)
      end

fun HcP_SoundGetRate(sound : HcP_Sound) : int =
      let
        val f = _import "mltHcP_SoundGetRate": int -> int;
      in
        f (HcPSound.toInt sound)
      end

fun HcP_SoundSetRepeat(sound : HcP_Sound, repeat : bool) : unit =
      let
        val f = _import "mltHcP_SoundSetRepeat": int * bool -> unit;
      in
        f (HcPSound.toInt sound, repeat)
      end

fun HcP_SoundPlay(sound : HcP_Sound) : unit =
      let
        val f = _import "mltHcP_SoundPlay": int -> unit;
      in
        f (HcPSound.toInt sound)
      end
