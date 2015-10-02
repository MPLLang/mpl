default: lami

lami:
	../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../build/lib/sml/basis/parallel/ws6-map -output int_test lami.mlb

shell: shell.sml lami
	../../build/bin/mlton -const 'Exn.keepHistory true' -default-ann 'allowFFI true' -mlb-path-map ../../build/lib/sml/basis/parallel/ws3-map -output shell shell.mlb

countcl: countcl.sml
	../../build/bin/mlton -default-ann 'allowFFI true' -link-opt -L/usr/X11R6/lib -link-opt -lX11 -mlb-path-map ../../build/lib/sml/basis/parallel/ws6-map countcl.mlb ../../../sml-lib/mlton/x/mlx.o