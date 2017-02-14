(* Written by ANOQ of the Sun <anoq@HardcoreProcessing.com>

   There is no warranty of any kind and I will not be
   held responsible for any problems it may cause.

   Use at your own risk! *)

fun waitForQuit sound =
      let
        val (code, value) = HcP_WaitEvent ()
      in
        if code = SDL_QUIT then
          ()
        else if code = SDL_MOUSEBUTTONDOWN then
          (HcP_SoundPlay sound; waitForQuit sound)
        else
          waitForQuit sound
      end

fun runTest sdlScreen =
      let
        val testImage = SDL_LoadBMP("test.bmp")
        val testSound = HcP_LoadWAV("test.wav")
        val _ = SDL_BlitSurface(testImage, 0, 0, 640, 480,
                                sdlScreen, 0, 0, 640, 480)
        val _ = SDL_UpdateRect(sdlScreen, 0, 0, 640, 480)

        val _ = waitForQuit testSound

        val _ = HcP_SoundFree(testSound)
        val _ = SDL_FreeSurface(testImage)
      in
        ()
      end


fun SDLTest () =
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

val _ = SDLTest()


