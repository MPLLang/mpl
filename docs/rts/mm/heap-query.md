---
layout: page
title: Heap Queries
parent: Memory Management
---

# Heap Queries

For any [Heap Object](object.html), we can efficiently query which
[Heap](heap.html) contains that object. Typically, this is implemented in
the code as follows: first we get the chunk containing the object, and then we
use the union-find acceleration structure to get to the "level head", i.e., the
representative `HM_HierarchicalHeap` structure for this region of memory.

```c
objptr op;              // given a pointer to some object
HM_HierarchicalHeap h;  // result is a pointer to a heap
h = HM_getLevelHead(HM_getChunkOf(op));
```

Below is an illustration of a full heap query.
① We start with an `objptr` pointing to some object.
② The function call `HM_getChunkOf(...)` returns the chunk descriptor
(see [Chunks](chunk.html)) for the chunk that contains that object. 
③ Next, with the chunk descriptor in hand, we call
`HM_getLevelHead` to traverse the union-find structure.
④ Finally, out of the union-find structure, we arrive at
the appropriate `HM_HierarchicalHeap`.

![Visualizing a heap query]({{site.baseurl}}/assets/heap-query.png){:width="100%"}

{: .note}
> Throughout the run-time system you will also see the following.
> ```c
> h = HM_getLevelHeadPathCompress(HM_getChunkOf(op));
> ```
> This is similar to the approach described above, except that
> it also performs path compression within the union-find structure to help
> accelerate future queries. Some care is needed to avoid concurrency issues,
> because `HM_getLevelHeadPathCompress` modifies the union-find structure.

It might at first seem like heap queries are expensive. In practice, a
typical heap query requires only a bitmask, two pointer dereferences, and a
couple conditionals. Path compression ensures that the number of links
traversed within the union-find structure is
[almost constant](https://en.wikipedia.org/wiki/Disjoint-set_data_structure#History).