---
layout: page
title: Forwarding Pointers
parent: Memory Management
---

# Forwarding Pointers

A **forwarding pointer** redirects from an old version of an object to a new
version. Forwarding pointers are created temporarily by
[Local Garbage Collection (LGC)](lgc.html) when copying live objects into fresh
heaps.

{: .note}
Assuming nothing has gone terribly wrong, forwarding pointers should
be "invisible" to the source program. In particular, during an LGC, the thread
that owns the heaps in-scope of the collection is paused. Any other thread
which attempts to access an object in-scope of the collection is protected
against witnessing a forwarding pointer by the
[Read Barrier](read-barrier.html).


To **forward** an object, we:
  1. copy the object to a new location, and
  2. overwrite the (old) object's [Header](header.html) with a pointer to the new location.

Note that forwarding pointers are stored in place of the [Header](header.html).
We can distinguish between a header and a forwarding
pointer by checking the least-significant bit. Every header will have a 1 in
the least-significant position; forwarding pointers will always have a 0
in this position, due to alignment constraints.

![Forwarding an object]({{site.baseurl}}/assets/forward.png){:width="80%"}