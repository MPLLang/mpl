default: fib_net

fib_net:
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6-map -output fib_net-ws6 fib_net.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6o-map -output fib_net-ws6o fib_net.mlb

uts_term:
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6-map -output uts2_term-ws6 uts_term.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6o-map -output uts2_term-ws6o uts_term.mlb

fib_term:
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6-map -output fib_term-ws6 fib_term.mlb
#	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6ts-map -output fib_term-ws6ts fib_term.mlb
#	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6tslc-map -output fib_term-ws6tslc fib_term.mlb
	../../../../mlton-parmem-master/build/bin/mlton-parmem -default-ann 'allowFFI true' -mlb-path-map ../../../../mlton-parmem-master/build/lib/sml/basis/parallel/ws6-map -output fib_term-ws6o fib_term.mlb
#	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6tw-map -output fib_term-ws6tw fib_term.mlb

uts_net:
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6-map -output uts2_net-ws6 uts_net.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6o-map -output uts2_net-ws6o uts_net.mlb

bfib:
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/bws-map -output fib-bws fib_term.mlb

file:
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/bws-map -output file-bws file.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6o-map -output file-o fileo.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/bws-map -output file-nb-bws fileo.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/bws-map -output file-seq fileseq.mlb

fib-nob:
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/bws-map -output fib-bws fib-nob.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6o-map -output fib-o fib-nob.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/bws-map -output fib-seq fibseq-nob.mlb

mfib-nob:
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/bws-map -output mfib-bws mfib-nob.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6o-map -output mfib-o mfib-nob.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/bws-map -output mfib-seq mfib-seq.mlb


baselines:
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6o-map -output uts uts.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6o-map -output fib fib.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6-map -output term term.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6-map -output net net.mlb

shell: shell.sml lami
	../../build/bin/mlton -const 'Exn.keepHistory true' -default-ann 'allowFFI true' -mlb-path-map ../../build/lib/sml/basis/parallel/ws3-map -output shell shell.mlb

countcl: countcl.sml
	../../build/bin/mlton -default-ann 'allowFFI true' -link-opt -L/usr/X11R6/lib -link-opt -lX11 -mlb-path-map ../../build/lib/sml/basis/parallel/ws6ts-map countcl.mlb ../../../sml-lib/mlton/x/mlx.o

anagrams: anagrams.sml
	../../build/bin/mlton -default-ann 'allowFFI true' -link-opt -L/usr/X11R6/lib -link-opt -lX11 -mlb-path-map ../../build/lib/sml/basis/parallel/ws6-map anagrams.mlb ../../../sml-lib/mlton/x/mlx.o

fib_term-pd: fib_term.sml ../../../build/lib/sml/basis/parallel/basicpd.sml
	../../../build/bin/mlton -default-ann 'allowFFI true' -cc-opt -g -mlb-path-map ../../../build/lib/sml/basis/parallel/pd-map -output fib_term-pd fib_term.mlb

fib_net-pd: fib_net.sml ../../../build/lib/sml/basis/parallel/basicpd.sml
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/pd-map -output fib_net-pd fib_net.mlb

uts_term-pd: uts_term.sml ../../../build/lib/sml/basis/parallel/basicpd.sml
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/pd-map -output uts_term-pd uts_term.mlb

uts_net-pd: uts_net.sml ../../../build/lib/sml/basis/parallel/basicpd.sml
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/pd-map -output uts_net-pd uts_net.mlb

net_test: net_test.sml ../../../build/lib/sml/basis/parallel/basicpd.sml
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6o-map -output net_test net_test.mlb

pd: fib_term-pd fib_net-pd uts_term-pd uts_net-pd

seq: seqfib.sml sequts.sml
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6o-map -output seqfib seqfib.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6o-map -output sequts sequts.mlb
