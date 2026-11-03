This "top down" Delaunay implementation is a direct translation of this file:
https://github.com/cmuparlay/parlaylib/blob/e1f1dc0ccf930492a2723f7fbef8510d35bf57f5/examples/delaunay.h

It is interesting algorithmically, but not
especially fast. I would be curious to see how well it performs in comparison
to the original C++ code.

In comparison to the [MaPLe PBBS `delaunay`](https://github.com/MPLLang/parallel-ml-bench/tree/main/mpl/bench/delaunay), it is significantly slower
and less space efficient. I believe this is partly due to the use of two
(somewhat unoptimized) global hash tables. One stores the mesh of triangles,
and the other stores a set of outstanding edges that may need to be processed.
Note that the use of hash tables in this way results in memory entanglement,
which is another source of overhead. Disentangling the hash table access
patterns would be interesting to look into; it's not immediately clear to me if
this is possible.

Some basic tests have been run but more testing is required.

```bash
[parallel-ml-bench/mpl]$ make delaunay-top-down.mpl.bin
[parallel-ml-bench/mpl]$ bin/delaunay-top-down.mpl.bin @mpl procs 4 -- -n 10000 -output result.ppm -resolution 2000

# ... see image in result.ppm
```

**NOTE (9/2/25)**: Currently, there is an unresolved race condition which
sometimes results in portions of the triangulation being dropped. It may be a
bug in the hash table implementation.