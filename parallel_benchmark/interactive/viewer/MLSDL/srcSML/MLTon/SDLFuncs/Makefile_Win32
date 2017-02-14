# Written by ANOQ of the Sun <anoq@HardcoreProcessing.com>

# There is no warranty of any kind and I will not be
# held responsible for any problems it may cause.

# Use at your own risk!

Source: ../../../srcC/MLTon/SDLFuncs/SDLFuncs.o
	mlton Source.sml ../../../srcC/MLTon/SDLFuncs/SDLFuncs.o -D_WIN32 -lmingw32 -mwindows -L/usr/local/lib -lSDL

#SDLFuncs.o: ../../../srcC/MLTon/SDLFuncs/SDLFuncs.c
#	mlton -I/usr/local/include/SDL -D_REENTRANT -c ../../../srcC/MLTon/SDLFuncs/SDLFuncs.c -o SDLFuncs.o

clean:
	rm -f Source Source.o
