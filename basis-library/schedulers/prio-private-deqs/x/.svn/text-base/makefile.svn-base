default : mlxtest

mlx.o : mlx.c
	gcc -O -Wall -Werror mlx.c -c -o mlx.o

mlxtest : *.sml mlx.o
	mlton -default-ann "allowImport true" -link-opt -L/usr/X11R6/lib -link-opt -lX11 mlxtest.cm mlx.o
	strip mlxtest

nj : *.sml *.cm
	cat mlx.sml | sed -e 's/_ffi/Unsafe.cast/' > mlx-nj.sml
	cat mlxtest.cm | sed -e 's/mlx.sml/mlx-nj.sml/' > sources.cm

typecheck : nj
	echo "CM.make();" | sml
