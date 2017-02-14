// Written by ANOQ of the Sun <anoq@HardcoreProcessing.com>

// There is no warranty of any kind and I will not be
// held responsible for any problems it may cause.

// Use at your own risk!

#include <malloc.h>
#include <SDL.h>
#include "HcPPixels.c"
#include "SDLPrinting.c"
//#include "tests.c"

// This file is compiled with the accompanying makefile:
//
// For Linux->Win32 crosscompilation:
// make target=win32
//
// For native Linux compilation:
// make

void myLogError(char *fileName, char *error)
{
    FILE *fp;
    fp = fopen(fileName, "w");
    fprintf(fp, "%s%s\n",error, SDL_GetError());
    fclose(fp);
}

int mltSDL_InitDefault(void)
{
    int result;
    
    result = SDL_Init(SDL_INIT_AUDIO | SDL_INIT_VIDEO);
    if(result < 0)
        myLogError("SDL_Init.log", "Could not initialize SDL: ");
    return result;
}

void mltSDL_Quit(void)
{
    SDL_Quit();
}

int mltSDL_SetVideoModeDefault(int width, int height, int bpp)
{
    // return type: SDL_Surface *
    // FIXME: This typecast is very dangerous and unsafe!!!
    return ((int)SDL_SetVideoMode(width, height, bpp,
                                  SDL_HWSURFACE/* | SDL_FULLSCREEN */));
}

int mltSDL_BlitSurface(int srcSurf, int srcX, int srcY, int srcW, int srcH,
                       int dstSurf, int dstX, int dstY, int dstW, int dstH)
{
    SDL_Rect sdlDest;
    SDL_Rect sdlSource;
    
    sdlDest.x = dstX;
    sdlDest.y = dstY;
    sdlDest.w = dstW;
    sdlDest.h = dstH;
    sdlSource.x = srcX;
    sdlSource.y = srcY;
    sdlSource.w = srcW;
    sdlSource.h = srcH;
    return (int)SDL_BlitSurface((SDL_Surface *)srcSurf,
                                &sdlSource,
                                (SDL_Surface *)dstSurf,
                                &sdlDest);
}

void mltSDL_UpdateRect(int surf, int x, int y, int w, int h)
{
    SDL_UpdateRect((SDL_Surface *)surf, x, y, w, h);
}

int mltSDL_LoadBMP(char *fileName)
{
    int res;
    
    res = (int)SDL_LoadBMP(fileName);
    if(!res)
        myLogError("SDL_LoadBMP.log", "Could not load BMP file: ");
    
    return res;
}

int mltSDL_AllocSurfaceDefaultAlpha(int width, int height, int depth,
                                    int Rmask, int Gmask, int Bmask, int Amask)
{
    return (int)SDL_AllocSurface(SDL_SRCALPHA, width, height, depth,
                                 (Uint32)Rmask, (Uint32)Gmask,
                                 (Uint32)Bmask, (Uint32)Amask);
}

void mltSDL_FreeSurface(int surf)
{
    SDL_FreeSurface((SDL_Surface *)surf);
}

/* This function may run in a separate event thread */
int eventFilter(const SDL_Event *event)
{
    return(1);//Accept anything
}

void mltSDL_SetEventFilterDefault(void)
{
    SDL_SetEventFilter(eventFilter);
}

int mltHcP_GetMouseButton(void)
{
  // XXX TODO This is broken
  int button = 0;
  Uint32 bm = SDL_GetMouseState(NULL, NULL);

  if (bm & SDL_BUTTON(1))
    return 1;
  if (bm & SDL_BUTTON(2))
    return 2;
  if (bm & SDL_BUTTON(3))
    return 3;

  return 0;
}

int mltHcP_GetMouseX(void)
{
    Uint16 mx, my;

    SDL_GetMouseState(&mx, &my);
    
    return mx;
}

int mltHcP_GetMouseY(void)
{
    Uint16 mx, my;

    SDL_GetMouseState(&mx, &my);
    
    return my;
}

int mltSDL_FillRect(int surf, int x, int y, int w, int h, int col)
{
    SDL_Rect sdlRect;
    
    sdlRect.x = x;
    sdlRect.y = y;
    sdlRect.w = w;
    sdlRect.h = h;
    return (int)SDL_FillRect((SDL_Surface *)surf, &sdlRect, (Uint32)col);
}

int mltSDL_MapRGB(int surf, int r, int g, int b)
{
    return (int)SDL_MapRGB(((SDL_Surface *)surf)->format, r, g, b);
}

int mltSDL_SetColorKeySource(int surf, int col)
{
    return (int)SDL_SetColorKey((SDL_Surface *)surf,
                                SDL_SRCCOLORKEY,
                                (Uint32)col);
}

int mltSDL_SetAlphaSource(int surf, int alpha)
{
    return (int)SDL_SetAlpha((SDL_Surface *)surf,
                             SDL_SRCALPHA,
                             (Uint8)alpha);
}

void mltHcP_PutPixel(int surf, int r, int g, int b, int x, int y)
{
    HcP_PutPixel((SDL_Surface *)surf, (Uint8)r, (Uint8)g, (Uint8)b, (Uint32)x, (Uint32)y);
}

int mltHcP_GetPixel(int surf, int x, int y)
{
    return (int)HcP_GetPixel((SDL_Surface *)surf, (Uint32)x, (Uint32)y);
}

int mltSDL_GetTicks(void)
{
    return (int)SDL_GetTicks();
}

void mltSDL_Delay(int milliSecs)
{
    SDL_Delay((Uint32)milliSecs);
}

void mltHcP_ExecuteFile(char *fileName)
{
#if defined(_WIN32)
    WinExec(fileName, SW_SHOWNORMAL);
#else
#endif
}

int mltHcP_GetImageWidth(int surf)
{
    return (int)(((SDL_Surface *)surf)->w);
}

int mltHcP_GetImageHeight(int surf)
{
    return (int)(((SDL_Surface *)surf)->h);
}

void mltSDL_SetPrinterDocTitle(char *title)
{
    SDL_SetPrinterDocTitle(title);
}

int mltSDL_GetPageWidth(void)
{
    return ((int)SDL_GetPageWidth());
}

int mltSDL_GetPageHeight(void)
{
    return ((int)SDL_GetPageHeight());
}

void mltSDL_AbortDoc(void)
{
    SDL_AbortDoc();
}

int mltSDL_BeginDoc(void)
{
    int result;

    result = (int)SDL_BeginDoc();
    if(!result)
        myLogError("SDL_BeginDoc.log", "Could not begin printing document: ");
    
    return ((int)result);
}

void mltSDL_EndDoc(void)
{
    SDL_EndDoc();
}

int mltSDL_NewPage(void)
{
    int result;

    result = (int)SDL_NewPage();
    if(!result)
        myLogError("SDL_NewPage.log", "Could not get new page while printing document: ");

    return ((int)result);
}

// My own simple functions
int inHcPHandleEvent(SDL_Event *event, int codeOrValue)
{
    int evCode = 0;
    int evValue = 0;

    switch(event->type)
    {
    case SDL_KEYDOWN:
        evCode = 2;
        evValue = (int)((SDL_KeyboardEvent *)event)->keysym.sym;
        break;
    case SDL_KEYUP:
        evCode = 3;
        evValue = (int)((SDL_KeyboardEvent *)event)->keysym.sym;
        break;
    case SDL_MOUSEMOTION:
        evCode = 4;
        break;
    case SDL_MOUSEBUTTONDOWN:
        evCode = 5;//evValue is set below
        break;
    case SDL_MOUSEBUTTONUP:
        evCode = 6;//evValue is set below
        break;
    case SDL_QUIT:
        evCode = 10;
        break;
    }

    if(evCode == 5 || evCode == 6)
    {
        switch (((SDL_MouseButtonEvent *)event)->button)
        {
        case SDL_BUTTON(1):
            evValue = 1;
            break;
        case SDL_BUTTON(2):
            evValue = 2;
            break;
        case SDL_BUTTON(3):
            evValue = 3;
            break;
        }
    }
    
    if(codeOrValue == 0)
        return evCode;
    else
        return evValue;
}

int mltSDL_WaitEvent(void)
{
    int result;
    static SDL_Event event;//FIXME: Only one event structure will ever exist!!!

    result = SDL_WaitEvent(&event);

    if(result)
        return ((int)(&event));
    else
        return 0;
}

int mltSDL_PollEvent(void)
{
    int result;
    static SDL_Event event;//FIXME: Only one event structure will ever exist!!!

    result = SDL_PollEvent(&event);

    if(result)
        return ((int)(&event));
    else
        return 0;
}

int mltHcP_EventCode(int event)
{
    if(event)
        return inHcPHandleEvent((SDL_Event *)event, 0);
    else
        return 0;
}

int mltHcP_EventValue(int event)
{
    if(event)
        return inHcPHandleEvent((SDL_Event *)event, 1);
    else
        return 0;
}

int mltHcP_DanishAE(void)
{
#ifdef __WIN32__
    return (int)SDLK_BACKQUOTE; //æ - danish keyboard (assumed here for win users...)
#else
    return (int)SDLK_SEMICOLON; //æ - american keyboard
#endif
}

int mltHcP_DanishOE(void)
{
    return (int)SDLK_QUOTE;//ø - on both american and danish keyboard
}

int mltHcP_DanishAA(void)
{
#ifdef __WIN32__
    return (int)SDLK_RIGHTBRACKET; //å - danish keyboard (assumed here for win users...)
#else
    return (int)SDLK_LEFTBRACKET;//å - american keyboard
#endif
}

// The rest is the Hardcore Processing Audio System (TM) :)
// It will just play multiple sounds at the same time - nothing fancy.

struct snd
{
    char *mData;
    char *mConvertedData;
    unsigned int mLength, mConvertedLength;
    SDL_AudioSpec mSpec;
    int mConverted;
    int mRepeat;
};

struct sndPlaying
{
    int mLen, mPos;
    char *mBuffer;
    int mRepeat;
};

#define qSize 20
struct sndPlaying *playingQueue[qSize];//FIXME: Dynamic?...
int qCount = 0;

int auOpened = 0;
SDL_AudioSpec auSpec;

#define SILENT_AUDIO (!auOpened)

int plBytesLeft(int index)
{
    return ((playingQueue[index])->mLen -
            (playingQueue[index])->mPos);
}

int plGetBytes(int index, int amount, char **buffer)
{
    struct sndPlaying *pl;

    pl = playingQueue[index];
    
    if(amount > plBytesLeft(index))
        amount = plBytesLeft(index);
    if(amount > 0)
    {
        
        *buffer = pl->mBuffer + pl->mPos;
        pl->mPos += amount;
        if(pl->mRepeat && plBytesLeft(index) <= 0)
            pl->mPos = 0;
    }
    return amount;
}

void qFreeQueue(void)
{
    int cnt;

    SDL_LockAudio();
    for(cnt = 0;cnt < qCount;cnt++)
        free(playingQueue[cnt]);
    SDL_UnlockAudio();

    qCount = 0;
}

// NOTICE: Remember to lock audio before calling this...
void qAdd(char *buffer, int len, int repeat)
{
    struct sndPlaying *res;
    
    if(qCount >= qSize) // FIXME: No dynamic grow...
        return;

    res = (struct sndPlaying *)malloc(sizeof(struct sndPlaying));

    SDL_LockAudio();
    playingQueue[qCount] = res;
    res->mPos = 0;
    res->mLen = len;
    res->mBuffer = buffer;
    res->mRepeat = repeat;
    qCount++;
    SDL_UnlockAudio();
    return;
}

extern void fillAudio(void *udata, Uint8 *stream, int len);

/* The audio function callback takes the following parameters:
    stream:  A pointer to the audio buffer to be filled
    len:     The length (in bytes) of the audio buffer
*/
void fillAudio(void *userdata, Uint8 *stream, int len)
{
    int cnt, gotBytes, toCnt;
    char *srcBuf;
    
    //memset(stream, 0, len); FIXME: Is the SDL-buffer empty?

    //This loop runs through the queue and mixes a buffer of
    //all the sounds. All sounds which are played to their
    //end will be removed from the queue
    toCnt = 0;
    for(cnt = 0;cnt < qCount;cnt++)
    {
        //Mix the buffer
        gotBytes = plGetBytes(cnt, len, &srcBuf);
        SDL_MixAudio((unsigned char *)stream, (unsigned char *)srcBuf,
                     gotBytes, SDL_MIX_MAXVOLUME);
        //Move pointers down the list if necessary
        if(toCnt != cnt)//Move pointers down the list if necessary
            playingQueue[toCnt] = playingQueue[cnt];

        if(plBytesLeft(cnt) == 0)
            free(playingQueue[cnt]);
        else
            toCnt++;//Keep this sound
    }
    qCount = toCnt;
}

int mltHcP_AudioBegin(void)
{
    int result;
    SDL_AudioSpec wantedSpec;
    
    if(auOpened)
        return (int)(-1);

    wantedSpec.freq = 22050;
    wantedSpec.format = AUDIO_S16;
    
    wantedSpec.samples = 64;//FIXME: This is for a responsive game but
    //but since values this small can lead to underflow in the stream
    //I believe there should be runtime correction of this value.
    wantedSpec.callback = fillAudio;
    wantedSpec.userdata = NULL;
    wantedSpec.channels = 2;

    /* Open the audio device */
    result = SDL_OpenAudio(&wantedSpec, &auSpec);
    if (result < 0 )
    {
        myLogError("SDL_OpenAudio.log", "Could not open audio: ");
        return ((int)result);
    }
    auOpened = 1;
    
    SDL_PauseAudio(0);

    return ((int)result);
}

void mltHcP_AudioEnd(void)
{
    if(auOpened)
    {
        SDL_PauseAudio(1);
        qFreeQueue();
        auOpened = 0;
    }
}

struct snd *sdCreate(void)
{
    struct snd *res;

    res = (struct snd *)malloc(sizeof(struct snd));
    res->mData = (char *)NULL;
    res->mConverted = 0;
    res->mRepeat = 0;

    return res;
}

void sdCheckFreeConvertedData(struct snd *s)
{
    if(s->mConverted)
    {
        free(s->mConvertedData);
        s->mConverted = 0;
    }
}

void sdCheckFreeData(struct snd *s)
{
    if(s->mData)
    {
        sdCheckFreeConvertedData(s);
        free(s->mData);
        s->mData = (char *)NULL;
    }
}

void mltHcP_SoundFree(int intS)
{
    struct snd *s = (struct snd *)intS;

    if(SILENT_AUDIO)
        return;
    
    sdCheckFreeData(s);
    free(s);
}

int mltHcP_LoadWAV(char *fileName)
{
    struct snd *s;
    
    if(SILENT_AUDIO)
        return 0;// This corresponds to a NULL-pointer, and it will just be quiet...
    
    s = sdCreate();
    if(!s)
        return 0;
    if(!SDL_LoadWAV((const char *)fileName, &s->mSpec,
                    (unsigned char **)&s->mData, &s->mLength))
    {
        myLogError("SDL_LoadWAV.log", "Could not open file: ");
        mltHcP_SoundFree((int)s);
        return 0;
    }
    return ((int)s);
}

void sdCheckConvert(struct snd *s)
{
    SDL_AudioCVT cvt;
    int convertResult;

    if(SILENT_AUDIO)
        return;
    
    if(s->mConverted)
        return;//Already converted

    if(SDL_BuildAudioCVT(&cvt,
                         s->mSpec.format,
                         s->mSpec.channels,
                         s->mSpec.freq,
                         auSpec.format,
                         auSpec.channels,
                         auSpec.freq) < 0)
    {
        myLogError("SDL_BuildAudioCVT.log", "Could not convert sound: ");
        return;
    }
    
    cvt.len = s->mLength;
    cvt.buf = (Uint8 *)malloc(cvt.len * cvt.len_mult);
    if(!cvt.buf)
        return; // FIXME: Not enough memory - raise exception?
    memcpy(cvt.buf, s->mData, cvt.len);//cvt.buf and mData may not overlap when using memcpy
    if(cvt.needed)
    {
        //Conversion needed
        if(SDL_ConvertAudio(&cvt) < 0)
        {
            free(cvt.buf);
            myLogError("SDL_ConvertAudio.log", "Could not convert sound: ");
            return;
        }
        s->mConvertedLength = cvt.len_cvt;
    }
    else
        s->mConvertedLength = s->mLength;
    
    s->mConvertedData = (char *)cvt.buf;
    s->mConverted = 1;
}

void mltHcP_SoundSetRate(int intS, int freq)
{
    struct snd *s;
    
    if(SILENT_AUDIO)
        return;

    s = (struct snd *)intS;

    if(!s->mData)
        return;//throw Error("Sound::setRate: No sound data!");

    s->mSpec.freq = freq;
    sdCheckFreeConvertedData(s);
}

int mltHcP_SoundGetRate(int intS)
{
    struct snd *s;
    
    if(SILENT_AUDIO)
        return (int)0;

    s = (struct snd *)intS;
    
    if(!s->mData)
        return (int)0;//throw Error("Sound::getRate: No sound data!");
    return (int)s->mSpec.freq;
}

void mltHcP_SoundSetRepeat(int intS, int repeat)
{
    struct snd *s;
    
    if(SILENT_AUDIO)
        return;

    s = (struct snd *)intS;
    
    s->mRepeat = repeat;
}

//Functions for playing / using sounds
void mltHcP_SoundPlay(int intS)
{
    struct snd *s;
    
    if(SILENT_AUDIO)
        return;

    s = (struct snd *)intS;

    sdCheckConvert(s);

    //FIXME: Should maybe return some "playing"-handle
    //  for manipulating the sound pitch, volume etc. during playback
    qAdd(s->mConvertedData, s->mConvertedLength, s->mRepeat);
}
