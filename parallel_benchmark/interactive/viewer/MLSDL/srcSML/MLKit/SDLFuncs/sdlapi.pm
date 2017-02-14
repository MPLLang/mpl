(* Written by ANOQ of the Sun <anoq@HardcoreProcessing.com>

   There is no warranty of any kind and I will not be
   held responsible for any problems it may cause.

   Use at your own risk! *)

import SDLFuncs.o:SDLFuncs-prof.o
in SDLFuncs.sml
end

(* flags for crosscompilation to Windows
   set it under the "Link with library" menu in MLKit. *)
(* "-lmingw32 -mwindows -L/usr/local/lib -lSDL" *)

    (* flags for compilation to Linux... *)
(* "-lpthread -L/usr/local/lib -Wl,-rpath,/usr/local/lib -lSDL -ldl -lm" *)