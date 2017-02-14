// Written by ANOQ of the Sun <anoq@HardcoreProcessing.com>

// There is no warranty of any kind and I will not be
// held responsible for any problems it may cause.

// Use at your own risk!

#include <SDL.h>

#if defined(_WIN32)
#include <windows.h>
#endif

// These are my printing routines for Win32
char printerDocTitle[32];
int printerAborted = 0;
int printing = 0;
int printerPageNumber = 0;

#if defined(_WIN32)
HDC printerDC = 0;
//DEVMODE *printerDevMode;
SDL_Surface *printerPrevSurf = NULL;

int
STDCALL abortProc(HDC printer, int error)
{
    return !printerAborted;
}
#else
#endif

int checkPrinting(int desired)
{
    if(desired == printing)
        return 1;

    if(printing)
        SDL_SetError("Error: Is still printing...");
    else
        SDL_SetError("Error: Is not printing...");

    return 0;
}

int fetchString(char **str, char **result)
{
    char *p;

    *result = *str;
    if(!*str)
        return 0;
    p = *str;
    while(*p == ' ')
        p++;
    *result = p;
    while(*p != '\0' && *p != ',')
        p++;
    if(*p == ',')
    {
        *p = '\0';
        p++;
        *str = p;
        return 1;
    }

    return 0;
}

void setState(int stIC)
{
#if defined(_WIN32)
    char defaultPrinter[80];
    char *cur, *device;

    if(printerDC)
        DeleteDC(printerDC);

    // Get default printer
    GetProfileString("windows", "device", "", defaultPrinter,
                     sizeof(defaultPrinter) - 1);
    cur = defaultPrinter;
    fetchString(&cur, &device);

    if(stIC)              //driver, device, port, printerDevMode
        printerDC = CreateIC(NULL, device, NULL,
                             (CONST DEVMODE *)NULL);// Not used
    else
        printerDC = CreateDC(NULL, device, NULL,
                             (CONST DEVMODE *)NULL);
    if(!printerDC)
    {
        SDL_SetError("Invalid printer!");
        return;
    }
#else
#endif
}

void SDL_SetPrinterDocTitle(char *newTitle)
{
    if(strlen(newTitle) > sizeof(printerDocTitle) - 1)
    {
        SDL_SetError("Length of printerdocument title is too long!");
        return;
    }
    strcpy(printerDocTitle, newTitle);
}

int SDL_GetPageHeight(void)
{
#if defined(_WIN32)
    //setState(true);
    return GetDeviceCaps(printerDC, VERTRES);
#else
    return 0;
#endif
}

int SDL_GetPageWidth(void)
{
#if defined(_WIN32)
    //setState(true);
    return GetDeviceCaps(printerDC, HORZRES);
#else
    return 0;
#endif
}

SDL_Surface *SDL_BeginDoc(void)
{
#if defined(_WIN32)
    SDL_Surface *result = NULL;
    DOCINFO doc;

    SDL_SetError("");
    if(checkPrinting(0))
    {
        setState(0);
        printing = 1;
        printerAborted = 0;
        printerPageNumber = 1;
        memset(&doc, 0, sizeof(DOCINFO));
        doc.cbSize = sizeof(DOCINFO);
        doc.lpszDocName = printerDocTitle;
        doc.lpszOutput = 0;
        SetAbortProc(printerDC, abortProc);
        StartDoc(printerDC, &doc);
        StartPage(printerDC);

        //FIXME: Init SDL_Surface with printerDC here

        printerPrevSurf = result;
        return result;
    }
    return NULL;
#else
    return NULL;
#endif
}

void SDL_EndDoc(void)
{
#if defined(_WIN32)
    if(checkPrinting(1))
    {
        //FIXME: Maybe SDL_FreeSurface should be updated?
        //SDL_FreeSurface(printerPrevSurf);
        printerPrevSurf = NULL;
        
        EndPage(printerDC);
        if(!printerAborted)
            EndDoc(printerDC);
        printing = 0;
        printerAborted = 0;
        printerPageNumber = 0;
    }
#else
#endif
}

void SDL_AbortDoc(void)
{
#if defined(_WIN32)
    if(checkPrinting(1))
    {
        //AbortDoc(printerDC);// FIXME: Is this missing from the Win32 API?
        printerAborted = 1;
        SDL_EndDoc();
    }
#else
#endif
}

SDL_Surface *SDL_NewPage(void)
{
#if defined(_WIN32)
    SDL_Surface *result = NULL;
    
    if(checkPrinting(1))
    {
        //FIXME: Maybe SDL_FreeSurface should be updated?
        //SDL_FreeSurface(printerPrevSurf);
        printerPrevSurf = NULL;
        
        EndPage(printerDC);
        StartPage(printerDC);
        printerPageNumber++;

        //FIXME: Init SDL_Surface with printerDC here

        printerPrevSurf = result;
        return result;
    }
    return NULL;
#else
    return NULL;
#endif
}

// FIXME: Should be called in SDL_Init
void SDL_SYS_PrintInit(void)
{
    *printerDocTitle = '\0';
}

// FIXME: Should be called in SDL_Quit
void SDL_SYS_PrintQuit(void)
{
#if defined(_WIN32)
    if(printing)
        SDL_EndDoc();
    if(printerDC)
        DeleteDC(printerDC);
#else
#endif

    //if(printerHandle)
    //    ClosePrinter(printerHandle);
}