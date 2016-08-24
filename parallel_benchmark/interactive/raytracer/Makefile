default: raytracer

raytracer:
	../../../build/bin/mlton -default-ann 'allowFFI true' -link-opt -L/usr/X11R6/lib -link-opt -lX11 -mlb-path-map ../../../build/lib/sml/basis/parallel/pd-map -output raytracer-pd raytracer.mlb ../x/mlx.o