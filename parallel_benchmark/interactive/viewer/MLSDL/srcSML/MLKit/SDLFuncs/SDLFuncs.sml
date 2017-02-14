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


type SDL_Surface = SDLSurface.t
type HcP_Sound = HcPSound.t

(* Test Imager *)
fun testimager(destSurf : SDL_Surface, srcSurf : SDL_Surface,
               destWidth : int, destHeight : int) : unit =
      prim ("@mltestimager", "@mltestimager",
            (SDLSurface.toInt destSurf,
             SDLSurface.toInt srcSurf,
             destWidth, destHeight))
(* End of test imager *)

(* SDL constants that we will need for SDL_Init... *)
(* val SDL_INIT_AUDIO = 0x0010
   val SDL_INIT_VIDEO = 0x0020 *)

(* This calls SDL_Init with SDL_INIT_AUDIO | SDL_INIT_VIDEO *)
fun SDL_InitDefault() : int =
      prim("@mlSDL_InitDefault", "@mlSDL_InitDefault", ())

fun SDL_Quit () : unit = 
      prim ("@mlSDL_Quit", "@mlSDL_Quit", ())

(* SDL constants that we will need for SDL_SetVideoMode... *)
(* val SDL_SWSURFACE = 0x00000000
   val SDL_FULLSCREEN = 0x80000000 FIXME: This will not fit in signed int...*)

(* This calls SDL_SetVideoMode with SDL_SWSURFACE | SDL_FULLSCREEN *)
fun SDL_SetVideoModeDefault(width : int, height : int,
                            bpp : int) : SDL_Surface =
      SDLSurface.fromInt
        (prim ("@mlSDL_SetVideoModeDefault",
               "@mlSDL_SetVideoModeDefault",
               (width, height, bpp)))

fun SDL_BlitSurface(srcSurf : SDL_Surface,
                    srcX : int, srcY : int,
                    srcW : int, srcH : int,
                    dstSurf : SDL_Surface,
                    dstX : int, dstY : int,
                    dstW : int, dstH : int) : int =
      prim ("@mlSDL_BlitSurface", "@mlSDL_BlitSurface",
            (SDLSurface.toInt srcSurf,
             srcX, srcY, srcW, srcH,
             SDLSurface.toInt dstSurf,
             dstX, dstY, dstW, dstH))

fun SDL_UpdateRect(surf : SDL_Surface,
                   x : int, y : int, w : int, h : int) : unit =
      prim("@mlSDL_UpdateRect", "@mlSDL_UpdateRect",
           (SDLSurface.toInt surf, x, y, w, h))

fun SDL_LoadBMP(fileName : string) : SDL_Surface =
      let
        val i = prim("mlSDL_LoadBMP", "mlSDL_LoadBMP",
                     (fileName, SDLMLError))
      in
        if i = 0 then
          raise SDLMLError
        else
          SDLSurface.fromInt i
      end

fun SDL_AllocSurfaceDefaultAlpha(width : int, height : int,
                                 depth : int, Rmask : int,
                                 Gmask : int, Bmask : int,
                                 Amask : int) : SDL_Surface =
      let
        val i = prim("mlSDL_AllocSurfaceDefaultAlpha",
                     "mlSDL_AllocSurfaceDefaultAlpha",
                     (width, height, depth, Rmask, Gmask, Bmask, Amask))
      in
        SDLSurface.fromInt i
      end

fun SDL_FreeSurface(surf : SDL_Surface) : unit =
      prim ("@mlSDL_FreeSurface", "@mlSDL_FreeSurface",
            SDLSurface.toInt surf)

fun SDL_SetEventFilterDefault() : unit =
      prim ("@mlSDL_SetEventFilterDefault",
            "@mlSDL_SetEventFilterDefault", ())

fun SDL_GetMouseState () : (int * (int * int)) =
      prim ("mlSDL_GetMouseState",
            "mlSDL_GetMouseState", ())

fun SDL_FillRect(surf : SDL_Surface,
                 x : int, y : int,
                 w : int, h : int, col : int) : int =
      prim ("@mlSDL_FillRect",
            "@mlSDL_FillRect",
            (SDLSurface.toInt surf, x, y, w, h, col))

fun SDL_MapRGB(surf : SDL_Surface, r : int, g : int, b : int) : int =
      prim ("@mlSDL_MapRGB", "@mlSDL_MapRGB",
            (SDLSurface.toInt surf, r, g, b))

fun SDL_SetColorKeySource(surf : SDL_Surface, col : int) : int =
      prim ("@mlSDL_SetColorKeySource",
            "@mlSDL_SetColorKeySource",
            (SDLSurface.toInt surf, col))

fun SDL_SetAlphaSource(surf : SDL_Surface, alpha : int) : int =
      prim ("@mlSDL_SetAlphaSource",
            "@mlSDL_SetAlphaSource",
            (SDLSurface.toInt surf, alpha))

fun HcP_PutPixel(surf : SDL_Surface, r : int, g : int, b : int, x : int, y : int) : unit =
      prim ("@mlHcP_PutPixel", "@mlHcP_PutPixel",
            (SDLSurface.toInt surf, r, g, b, x, y))

fun HcP_GetPixel(surf : SDL_Surface, x : int, y : int) : int =
      prim ("@mlHcP_GetPixel", "@mlHcP_GetPixel",
            (SDLSurface.toInt surf, x, y))

fun SDL_GetTicks () : int =
      prim ("@mlSDL_GetTicks",
            "@mlSDL_GetTicks", ())

fun SDL_Delay (milliSecs : int) : unit =
      prim ("@mlSDL_Delay",
            "@mlSDL_Delay", milliSecs)

fun HcP_ExecuteFile (fileName : string) : unit =
      prim ("mlHcP_ExecuteFile",
            "mlHcP_ExecuteFile",
            (fileName, SDLMLError))

fun HcP_GetImageWidth (surf : SDL_Surface) : int =
      prim ("@mlHcP_GetImageWidth",
            "@mlHcP_GetImageWidth",
            (SDLSurface.toInt surf))

fun HcP_GetImageHeight (surf : SDL_Surface) : int =
      prim ("@mlHcP_GetImageHeight",
            "@mlHcP_GetImageHeight",
            (SDLSurface.toInt surf))

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

fun SDL_SetPrinterDocTitle(title) : unit =
      prim ("mlSDL_SetPrinterDocTitle",
            "mlSDL_SetPrinterDocTitle", (title, SDLMLError))

fun SDL_GetPageWidth () : int =
      prim ("@mlSDL_GetPageWidth",
            "@mlSDL_GetPageWidth", ())

fun SDL_GetPageHeight () : int =
      prim ("@mlSDL_GetPageHeight",
            "@mlSDL_GetPageHeight", ())

fun SDL_AbortDoc () : unit =
      prim ("@mlSDL_AbortDoc",
            "@mlSDL_AbortDoc", ())

fun SDL_BeginDoc () : SDL_Surface =
      SDLSurface.fromInt
        (prim ("@mlSDL_BeginDoc",
               "@mlSDL_BeginDoc", ()))

fun SDL_EndDoc () : unit =
      prim ("@mlSDL_EndDoc",
            "@mlSDL_EndDoc", ())

fun SDL_NewPage () : SDL_Surface =
      SDLSurface.fromInt
        (prim ("@mlSDL_NewPage",
               "@mlSDL_NewPage", ()))

fun HcP_WaitEvent () : (int * int) =
      prim ("mlHcP_WaitEvent", "mlHcP_WaitEvent", ())

fun HcP_PollEvent () : (int * int) =
      prim ("mlHcP_PollEvent", "mlHcP_PollEvent", ())

fun HcP_DanishAE () : int =
      prim ("mlHcP_DanishAE",
            "mlHcP_DanishAE", ())

fun HcP_DanishOE () : int =
      prim ("mlHcP_DanishOE",
            "mlHcP_DanishOE", ())

fun HcP_DanishAA () : int =
      prim ("mlHcP_DanishAA",
            "mlHcP_DanishAA", ())

(* The rest is the Hardcore Processing Audio System (TM) :) *)
(* It will just play multiple sounds at the same time - nothing fancy. *)

fun HcP_AudioBegin() : int =
      prim ("@mlHcP_AudioBegin",
            "@mlHcP_AudioBegin", ())

fun HcP_AudioEnd() : unit =
      prim ("@mlHcP_AudioEnd",
            "@mlHcP_AudioEnd", ())

fun HcP_SoundFree(sound : HcP_Sound) : unit =
      prim ("@mlHcP_SoundFree",
            "@mlHcP_SoundFree",
            HcPSound.toInt sound)

fun HcP_LoadWAV(fileName : string) : HcP_Sound =
      let
        val sound = prim ("mlHcP_LoadWAV",
                          "mlHcP_LoadWAV",
                          (fileName, SDLMLError))
      in
        HcPSound.fromInt sound
      end

fun HcP_SoundSetRate(sound : HcP_Sound, freq : int) : unit =
      prim ("@mlHcP_SoundSetRate",
            "@mlHcP_SoundSetRate",
            (HcPSound.toInt sound, freq))

fun HcP_SoundGetRate(sound : HcP_Sound) : int =
      prim ("@mlHcP_SoundGetRate",
            "@mlHcP_SoundGetRate",
            HcPSound.toInt sound)

fun HcP_SoundSetRepeat(sound : HcP_Sound, repeat : bool) : unit =
      prim ("@mlHcP_SoundSetRepeat",
            "@mlHcP_SoundSetRepeat",
            (HcPSound.toInt sound, repeat))

fun HcP_SoundPlay(sound : HcP_Sound) : unit =
      prim ("@mlHcP_SoundPlay",
            "@mlHcP_SoundPlay",
            HcPSound.toInt sound)
