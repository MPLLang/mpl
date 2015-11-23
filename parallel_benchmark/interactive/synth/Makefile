default: fib_net

fib_net:
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6-map -output fib_net-ws6 fib_net.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6o-map -output fib_net-ws6o fib_net.mlb

uts_term:
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6-map -output uts2_term-ws6 uts_term.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6o-map -output uts2_term-ws6o uts_term.mlb

fib_term:
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6-map -output fib_term-ws6 fib_term.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6o-map -output fib_term-ws6o fib_term.mlb

uts_net:
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6-map -output uts2_net-ws6 uts_net.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6o-map -output uts2_net-ws6o uts_net.mlb

baselines:
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6-nop-map -output fib-ws6 fib.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6-nop-map -output uts2-ws6 uts.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6-map -output term term.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6-map -output net net.mlb

shell: shell.sml lami
	../../build/bin/mlton -const 'Exn.keepHistory true' -default-ann 'allowFFI true' -mlb-path-map ../../build/lib/sml/basis/parallel/ws3-map -output shell shell.mlb

countcl: countcl.sml
	../../build/bin/mlton -default-ann 'allowFFI true' -link-opt -L/usr/X11R6/lib -link-opt -lX11 -mlb-path-map ../../build/lib/sml/basis/parallel/ws6ts-map countcl.mlb ../../../sml-lib/mlton/x/mlx.o

anagrams: anagrams.sml
	../../build/bin/mlton -default-ann 'allowFFI true' -link-opt -L/usr/X11R6/lib -link-opt -lX11 -mlb-path-map ../../build/lib/sml/basis/parallel/ws6-map anagrams.mlb ../../../sml-lib/mlton/x/mlx.o