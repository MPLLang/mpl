default: ws6

ws6:
	../../../build/bin/mlton -default-ann 'allowFFI true' -link-opt -L/usr/X11R6/lib -link-opt -lX11 -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6-map -output raytracer-ws6 raytracer.mlb ../x/mlx.o

pd:
	../../../build/bin/mlton -default-ann 'allowFFI true' -link-opt -L/usr/X11R6/lib -link-opt -lX11 -link-opt -lSDL -mlb-path-map ../../../build/lib/sml/basis/parallel/pd-map -output raytracer-pd raytracer.mlb ../x/mlx.o ../viewer/MLSDL/srcC/MLTon/SDLFuncs/SDLFuncs.o