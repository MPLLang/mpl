(* Written by ANOQ of the Sun <anoq@HardcoreProcessing.com>

   There is no warranty of any kind and I will not be
   held responsible for any problems it may cause.

   Use at your own risk! *)

fun runTest sdlScreen =
      let
        val _ = SDL_SetPrinterDocTitle("printer test")
        val surf = SDL_BeginDoc()
        val _ = print ("Width: " ^ (Int.toString (SDL_GetPageWidth())) ^
                       " Height: " ^ (Int.toString (SDL_GetPageHeight())) ^
                       "\n")
        val _ = SDL_EndDoc()
      in
        ()
      end

fun TestPrint () =
  if SDL_InitDefault () < 0 then
    ()
  else
    let
      val sdlScreen = SDL_SetVideoModeDefault(640, 480, 32)
      val _ = HcP_AudioBegin ()
      val _ = runTest sdlScreen
      val _ = HcP_AudioEnd ()
      val _ = SDL_Quit()
    in
      ()
    end

val _ = TestPrint()