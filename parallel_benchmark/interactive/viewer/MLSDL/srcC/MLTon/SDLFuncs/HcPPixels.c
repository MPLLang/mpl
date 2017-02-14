// Written by ANOQ of the Sun <anoq@HardcoreProcessing.com>

// There is no warranty of any kind and I will not be
// held responsible for any problems it may cause.

// Use at your own risk!

#include <SDL.h>

void HcP_PutPixel(SDL_Surface *surf, Uint8 R, Uint8 G, Uint8 B, Uint32 x, Uint32 y)
{
    Uint32 color = SDL_MapRGB(surf->format, R, G, B);

    if ( SDL_MUSTLOCK(surf) ) {
        if ( SDL_LockSurface(surf) < 0 ) {
            return;
        }
    }

    switch (surf->format->BytesPerPixel)
    {
    case 1: { /* Assuming 8-bpp */
        Uint8 *bufp;

        bufp = (Uint8 *)surf->pixels + y*surf->pitch + x;
        *bufp = color;
    }
    break;

    case 2: { /* Probably 15-bpp or 16-bpp */
        Uint16 *bufp;

        bufp = (Uint16 *)surf->pixels + y*surf->pitch/2 + x;
        *bufp = color;
    }
    break;

    case 3: { /* Slow 24-bpp mode, usually not used */
        Uint8 *bufp;

        bufp = (Uint8 *)surf->pixels + y*surf->pitch + x;
        *(bufp+surf->format->Rshift/8) = R;
        *(bufp+surf->format->Gshift/8) = G;
        *(bufp+surf->format->Bshift/8) = B;
    }
    break;

    case 4: { /* Probably 32-bpp */
        Uint32 *bufp;

        bufp = (Uint32 *)surf->pixels + y*surf->pitch/4 + x;
        *bufp = color;
    }
    break;
    }

    if ( SDL_MUSTLOCK(surf) ) {
        SDL_UnlockSurface(surf);
    }
}

void PutPixels(SDL_Surface *surf, Uint32 *pixels, int len)
{
    if ( SDL_MUSTLOCK(surf) ) {
      if ( SDL_LockSurface(surf) < 0 ) {
        return;
      }
    }
    Uint32 *bufp = (Uint32 *)surf->pixels;
    memcpy(bufp, pixels, len);
    if ( SDL_MUSTLOCK(surf) ) {
      SDL_UnlockSurface(surf);
    }
}

Uint32 HcP_GetPixel(SDL_Surface *surf, Uint32 x, Uint32 y)
{
    Uint32 result;
    
    if ( SDL_MUSTLOCK(surf) ) {
        if ( SDL_LockSurface(surf) < 0 ) {
            return;
        }
    }
    switch (surf->format->BytesPerPixel)
    {
    case 1: { /* Assuming 8-bpp */
        Uint8 *bufp;

        bufp = (Uint8 *)surf->pixels + y*surf->pitch + x;
        result = *bufp;
    }
    break;

    case 2: { /* Probably 15-bpp or 16-bpp */
        Uint16 *bufp;

        bufp = (Uint16 *)surf->pixels + y*surf->pitch/2 + x;
        result = *bufp;
    }
    break;

    case 3: { /* Slow 24-bpp mode, usually not used */
        Uint8 *bufp;

        //FIXME: Only 8 bit supported...
        
        bufp = (Uint8 *)surf->pixels + y*surf->pitch + x;
        result = ((*(bufp+surf->format->Rshift/8)>>surf->format->Rloss)<<surf->format->Rshift) |
		 ((*(bufp+surf->format->Gshift/8)>>surf->format->Gloss)<<surf->format->Gshift) |
		 ((*(bufp+surf->format->Bshift/8)>>surf->format->Bloss)<<surf->format->Bshift);
    }
    break;

    case 4: { /* Probably 32-bpp */
        Uint32 *bufp;

        bufp = (Uint32 *)surf->pixels + y*surf->pitch/4 + x;
        result = *bufp;
    }
    break;
    }

    if ( SDL_MUSTLOCK(surf) ) {
        SDL_UnlockSurface(surf);
    }
    return result;
}
