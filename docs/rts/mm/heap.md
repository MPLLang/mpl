---
layout: page
title: Hierarchical Heaps
parent: Memory Management
---

# Hierarchical Heaps

One of the key data structures in the MPL runtime system is the
**hierarchical heap**, which we often refer to simply as just a **heap**.
At any moment during execution there will be many heaps. Each heap is a
dynamically resizable memory region that contains [Heap Objects](object.html)
allocated during execution. Heaps do not overlap---every object is contained
within exactly one heap.

Heaps are organized in a tree structure which mirrors the fork-join task
structure of the program. This tree of heaps, called the **heap hierarchy**,
is a dynamic tree which grows and shrinks during execution, as tasks fork and
join.

Heaps are represented by the type [`HM_HierarchicalHeap`](https://github.com/MPLLang/mpl/blob/b64c5d2fb887768a80d010c9de6cc96ec41a4ba6/runtime/gc/hierarchical-heap.h#L50)
in `runtime/gc/hierarchical-heap.h`. A companion type,
[`HM_UnionFindNode`](https://github.com/MPLLang/mpl/blob/b64c5d2fb887768a80d010c9de6cc96ec41a4ba6/runtime/gc/hierarchical-heap.h#L17), is used to accelerate [Heap Queries](heap-query.html)
and also keep track of old `HM_HierarchicalHeap` structs that can be
reclaimed.

TODO...