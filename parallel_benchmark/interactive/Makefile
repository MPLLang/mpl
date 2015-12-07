default: lami

lami:
#	../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../build/lib/sml/basis/parallel/ws6-fib-map -output fib-ws6 fib.mlb
#	../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../build/lib/sml/basis/parallel/ws6-map -output int_test-ws6 lami.mlb
	../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../build/lib/sml/basis/parallel/ws6ts-map -output int_test-ws6ts lami.mlb
#	../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../build/lib/sml/basis/parallel/ws6tslc-map -output int_test-ws6tslc lami.mlb

qhull: qhull.sml
	../../build/bin/mlton -default-ann 'allowFFI true' -link-opt -L/usr/X11R6/lib -link-opt -lX11 -mlb-path-map ../../build/lib/sml/basis/parallel/ws6-map qhull.mlb ../../../sml-lib/mlton/x/mlx.o

fibserv: fibserv.sml
	../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../build/lib/sml/basis/parallel/ws6-map -output fibserv-ws6 fibserv.mlb
	../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../build/lib/sml/basis/parallel/ws6o-map -output fibserv-ws6o fibserv.mlb

game: gamed.sml
	../../build/bin/mlton -default-ann 'allowFFI true' -link-opt -L/usr/X11R6/lib -link-opt -lX11 -mlb-path-map ../../build/lib/sml/basis/parallel/ws6-map game.mlb ../../../sml-lib/mlton/x/mlx.o

shell: lami.sml sys.sml shell.sml
	../../build/bin/mlton -const 'Exn.keepHistory true' -default-ann 'allowFFI true' -mlb-path-map ../../build/lib/sml/basis/parallel/ws6-map -output shell shell.mlb

countcl: countcl.sml
	../../build/bin/mlton -default-ann 'allowFFI true' -link-opt -L/usr/X11R6/lib -link-opt -lX11 -mlb-path-map ../../build/lib/sml/basis/parallel/ws6ts-map countcl.mlb ../../../sml-lib/mlton/x/mlx.o

anagrams: anagrams.sml
	../../build/bin/mlton -default-ann 'allowFFI true' -link-opt -L/usr/X11R6/lib -link-opt -lX11 -mlb-path-map ../../build/lib/sml/basis/parallel/ws6-map anagrams.mlb ../../../sml-lib/mlton/x/mlx.o