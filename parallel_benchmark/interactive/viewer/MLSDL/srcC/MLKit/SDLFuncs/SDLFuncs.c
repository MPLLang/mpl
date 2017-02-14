// Written by ANOQ of the Sun <anoq@HardcoreProcessing.com>

// There is no warranty of any kind and I will not be
// held responsible for any problems it may cause.

// Use at your own risk!

#include <malloc.h>
#include <SDL.h>
#include "SDLPrinting.c"
#include "HcPPixels.c"

//Change these paths for your system
#include "/usr/local/devtools/ML/MLKit3_ToWin/src/Runtime/List.h"
#include "/usr/local/devtools/ML/MLKit3_ToWin/src/Runtime/String.h"
#include "/usr/local/devtools/ML/MLKit3_ToWin/src/Runtime/Exception.h"
#include "/usr/local/devtools/ML/MLKit3_ToWin/src/Runtime/Region.h"
#include "/usr/local/devtools/ML/MLKit3_ToWin/src/Runtime/Tagging.h"

// This file is compiled with the accompanying makefile:
//
// For Linux->Win32 crosscompilation:
// make target=win32
//
// For native Linux compilation:
// make

// Autoconversion works for functions with
// arguments of types int or bool and result type of int, bool or unit


void myLogError(char *fileName, char *error)
{
    FILE *fp;
    fp = fopen(fileName, "w");
    fprintf(fp, "%s%s\n",error, SDL_GetError());
    fclose(fp);
}

// Assumes that auto convertion is used.
int mlSDL_InitDefault(void)
{
    int result;
    
    result = SDL_Init(SDL_INIT_AUDIO | SDL_INIT_VIDEO);
    if(result < 0)
        myLogError("SDL_Init.log", "Could not initialize SDL: ");
    return result;
}

//This will hopefully work without auto conversion
int mlSDL_Quit(void)
{
    SDL_Quit();
    return;
}

int mlSDL_SetVideoModeDefault(int width, int height, int bpp)
{
    // return type: SDL_Surface *
    // FIXME: This typecast is very dangerous and unsafe!!!
    return ((int)SDL_SetVideoMode(width, height, bpp,
                                  SDL_HWSURFACE/* | SDL_FULLSCREEN */));
}

int mlSDL_BlitSurface(int srcSurf, int srcX, int srcY, int srcW, int srcH,
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

int mlSDL_UpdateRect(int surf, int x, int y, int w, int h)
{
    SDL_UpdateRect((SDL_Surface *)surf, x, y, w, h);
    return;
}

int mlSDL_LoadBMP(StringDesc *mlStr, int exn)
{
    int res;
    
    char fileName[1024];//FIXME: Let's just hope this won't be exceeded for now...
    
    convertStringToC(mlStr, fileName, sizeof(fileName), exn);
    //FIXME: Check for exception...

    res = (int)SDL_LoadBMP(fileName);
    if(!res)
        myLogError("SDL_LoadBMP.log", "Could not load BMP file: ");
    
    return (convertIntToML(res));
}

int mlSDL_AllocSurfaceDefaultAlpha(int width, int height, int depth,
                                   int Rmask, int Gmask, int Bmask, int Amask)
{
    return (int)SDL_AllocSurface(SDL_SRCALPHA, width, height, depth,
                                 (Uint32)Rmask, (Uint32)Gmask,
                                 (Uint32)Bmask, (Uint32)Amask);
}

int mlSDL_FreeSurface(int surf)
{
    SDL_FreeSurface((SDL_Surface *)surf);
    return;
}

/* This function may run in a separate event thread */
int eventFilter(const SDL_Event *event)
{
    return(1);//Accept anything
}

int mlSDL_SetEventFilterDefault(void)
{
    SDL_SetEventFilter(eventFilter);
    return;
}

int mlSDL_GetMouseState(int vAddr, int vAddr2)
{
    int button = 0;
    Uint16 mx, my;
    
    switch(SDL_GetMouseState(&mx, &my))
    {
    case SDL_BUTTON(1):
        button = 1;
        break;
    case SDL_BUTTON(2):
        button = 2;
        break;
    case SDL_BUTTON(3):
        button = 3;
        break;
    }

    //FIXME: I hope this is right...
    elemRecordML(vAddr, 0) = convertIntToML(button);
    elemRecordML(vAddr, 1) = vAddr2;
    elemRecordML(vAddr2, 0) = convertIntToML((int)mx);
    elemRecordML(vAddr2, 1) = convertIntToML((int)my);
    
    return vAddr;
}

int mlSDL_FillRect(int surf, int x, int y, int w, int h, int col)
{
    SDL_Rect sdlRect;
    
    sdlRect.x = x;
    sdlRect.y = y;
    sdlRect.w = w;
    sdlRect.h = h;
    return (int)SDL_FillRect((SDL_Surface *)surf, &sdlRect, (Uint32)col);
}

int mlSDL_MapRGB(int surf, int r, int g, int b)
{
    return (int)SDL_MapRGB(((SDL_Surface *)surf)->format, r, g, b);
}

int mlSDL_SetColorKeySource(int surf, int col)
{
    return (int)SDL_SetColorKey((SDL_Surface *)surf,
                                SDL_SRCCOLORKEY,
                                (Uint32)col);
}

int mlSDL_SetAlphaSource(int surf, int alpha)
{
    return (int)SDL_SetAlpha((SDL_Surface *)surf,
                             SDL_SRCALPHA,
                             (Uint8)alpha);
}

int mlHcP_PutPixel(int surf, int r, int g, int b, int x, int y)
{
    HcP_PutPixel((SDL_Surface *)surf, (Uint8)r, (Uint8)g, (Uint8)b, (Uint32)x, (Uint32)y);
    return;
}

int mlHcP_GetPixel(int surf, int x, int y)
{
    return (int)HcP_GetPixel((SDL_Surface *)surf, (Uint32)x, (Uint32)y);
}

int mlSDL_GetTicks(void)
{
    return (int)SDL_GetTicks();
}

int mlSDL_Delay(int milliSecs)
{
    SDL_Delay((Uint32)milliSecs);
    return;
}

int mlHcP_ExecuteFile(StringDesc *mlStr, int exn)
{
#if defined(_WIN32)
    char fileName[1024];//FIXME: Let's just hope this won't be exceeded for now...
    
    convertStringToC(mlStr, fileName, sizeof(fileName), exn);
    //FIXME: Check for exception...
    
    WinExec(fileName, SW_SHOWNORMAL);
#else
#endif
    return;
}

int mlHcP_GetImageWidth(int surf)
{
    return (int)(((SDL_Surface *)surf)->w);
}

int mlHcP_GetImageHeight(int surf)
{
    return (int)(((SDL_Surface *)surf)->h);
}

int mlSDL_SetPrinterDocTitle(StringDesc *mlStr, int exn)
{
    int res;
    
    char title[1024];//FIXME: Let's just hope this won't be exceeded for now...
    
    convertStringToC(mlStr, title, sizeof(title), exn);
    //FIXME: Check for exception...

    SDL_SetPrinterDocTitle(title);
    return;
}

int mlSDL_GetPageWidth(void)
{
    return ((int)SDL_GetPageWidth());
}

int mlSDL_GetPageHeight(void)
{
    return ((int)SDL_GetPageHeight());
}

int mlSDL_AbortDoc(void)
{
    SDL_AbortDoc();
    return;
}

int mlSDL_BeginDoc(void)
{
    int result;

    result = (int)SDL_BeginDoc();
    if(!result)
        myLogError("SDL_BeginDoc.log", "Could not begin printing document: ");
    
    return ((int)result);
}

int mlSDL_EndDoc(void)
{
    SDL_EndDoc();
    return;
}

int mlSDL_NewPage(void)
{
    int result;

    result = (int)SDL_NewPage();
    if(!result)
        myLogError("SDL_NewPage.log", "Could not get new page while printing document: ");

    return ((int)result);
}

// My own simple functions
int inHcPHandleEvent(int result, SDL_Event *event, int vAddr)
{
    int evCode = 0;
    int evValue = 0;

    if(result)
    {
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
    }
    elemRecordML(vAddr, 0) = convertIntToML(evCode);
    elemRecordML(vAddr, 1) = convertIntToML(evValue);
    return vAddr;
}

int mlHcP_WaitEvent(int vAddr)
{
    int result;
    SDL_Event event;

    result = SDL_WaitEvent(&event);
    return inHcPHandleEvent(result, &event, vAddr);
}

int mlHcP_PollEvent(int vAddr)
{
    int result;
    SDL_Event event;

    result = SDL_PollEvent(&event);
    return inHcPHandleEvent(result, &event, vAddr);
}

int mlHcP_DanishAE(void)
{
#ifdef __WIN32__
    return (int)SDLK_BACKQUOTE; //æ - danish keyboard (assumed here for win users...)
#else
    return (int)SDLK_SEMICOLON; //æ - american keyboard
#endif
}

int mlHcP_DanishOE(void)
{
    return (int)SDLK_QUOTE;//ø - on both american and danish keyboard
}

int mlHcP_DanishAA(void)
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

int mlHcP_AudioBegin(void)
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

int mlHcP_AudioEnd(void)
{
    if(auOpened)
    {
        SDL_PauseAudio(1);
        qFreeQueue();
        auOpened = 0;
    }
    return;
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

int mlHcP_SoundFree(int intS)
{
    struct snd *s = (struct snd *)intS;

    if(SILENT_AUDIO)
        return;
    
    sdCheckFreeData(s);
    free(s);
    return;
}

int mlHcP_LoadWAV(StringDesc *mlStr, int exn)
{
    char fileName[1024];//FIXME: Let's just hope this won't be exceeded for now...
    struct snd *s;
    
    if(SILENT_AUDIO)
        return convertIntToML(0);// This corresponds to a NULL-pointer, and it will just be quiet...
    
    convertStringToC(mlStr, fileName, sizeof(fileName), exn);
    //FIXME: Check for exception...

    s = sdCreate();
    if(!s)
        return convertIntToML(0);
    if(!SDL_LoadWAV((const char *)fileName, &s->mSpec,
                    (unsigned char **)&s->mData, &s->mLength))
    {
        myLogError("SDL_LoadWAV.log", "Could not open file: ");
        mlHcP_SoundFree((int)s);
        return convertIntToML(0);
    }
    return convertIntToML((int)s);
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

int mlHcP_SoundSetRate(int intS, int freq)
{
    struct snd *s;
    
    if(SILENT_AUDIO)
        return;

    s = (struct snd *)intS;

    if(!s->mData)
        return;//throw Error("Sound::setRate: No sound data!");

    s->mSpec.freq = freq;
    sdCheckFreeConvertedData(s);
    return;
}

int mlHcP_SoundGetRate(int intS)
{
    struct snd *s;
    
    if(SILENT_AUDIO)
        return (int)0;

    s = (struct snd *)intS;
    
    if(!s->mData)
        return (int)0;//throw Error("Sound::getRate: No sound data!");
    return (int)s->mSpec.freq;
}

int mlHcP_SoundSetRepeat(int intS, int repeat)
{
    struct snd *s;
    
    if(SILENT_AUDIO)
        return;

    s = (struct snd *)intS;
    
    s->mRepeat = repeat;
    return;
}

//Functions for playing / using sounds
int mlHcP_SoundPlay(int intS)
{
    struct snd *s;
    
    if(SILENT_AUDIO)
        return;

    s = (struct snd *)intS;

    sdCheckConvert(s);

    //FIXME: Should maybe return some "playing"-handle
    //  for manipulating the sound pitch, volume etc. during playback
    qAdd(s->mConvertedData, s->mConvertedLength, s->mRepeat);
    return;
}
