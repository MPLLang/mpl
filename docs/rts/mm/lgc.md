---
layout: page
title: Local Garbage Collection
parent: Memory Management
---

# Local Garbage Collection (LGC)

In the [Heap Hierarchy](heap.html), leaf heaps (and nearby ancestors, generally
deep in the hierarchy) are subjected to a hybrid copying/in-place collection
algorithm called **Local Garbage Collection**, or **LGC** for short.
The algorithm is hybrid in the sense that certain "pinned" objects are kept
in-place but all other live objects are forwarded (copied) into fresh heaps.
The forwarding component of the algorithm is an adaptation of
[Cheney's classic algorithm](https://en.wikipedia.org/wiki/Cheney%27s_algorithm).

{: .todo}
...