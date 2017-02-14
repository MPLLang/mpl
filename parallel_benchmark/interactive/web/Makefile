default: pd

ws6: web.sml web.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6-map -output web-ws6 web.mlb

ws6o: web.sml web.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/ws6o-map -output web-ws6o web.mlb

pd: web.sml web.mlb
	../../../build/bin/mlton -default-ann 'allowFFI true' -mlb-path-map ../../../build/lib/sml/basis/parallel/pd-map -output web-pd web.mlb