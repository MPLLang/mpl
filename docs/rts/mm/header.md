---
layout: page
title: Object Headers
parent: Memory Management
---

# Object Headers

Every [Heap Object](object.html) has an 8-byte **header** which contains
information about the object size, field layout, etc. This is implemented
with the type [`GC_header`](https://github.com/MPLLang/mpl/blob/6d7bb8588db713b87c515725bff3b5589fe8a9ad/runtime/gc/object.h#L53) in the run-time system.

As of MaPLe v0.5, the header layout is as follows (in order of least-significant
to most-significant bit).
  * Bit 0: indicates a **valid header**. It is set to 0 only if
    the header has been overwritten with a [Forwarding Pointer](fwd-ptr.html).
  * Bits 1-19: the **object type index**. The object type can be
    retrieved by indexing into the [Object Type Table](obj-type-table.html).
  * Bits 20-30 are used for [Entanglement Detection and Management](../em):
      * Bits 20-27: the **object unpin depth**, used to track when a pinned object
        can be safely unpinned.
      * Bits 28-29: the **pin type** of the object, used to distinguish between
        pinned and unpinned objects.
      * Bit 30: indicates whether or not the object is an **entanglement suspect**.
  * Bit 31: unused.

![Header bit layout]({{site.baseurl}}/assets/header.png){:width="80%"}

{: .note}
> Only the 4 lowest bytes of the header are currently used. This
> is a holdover from MLton, which supports both 32-bit and 64-bit execution
> modes. In 32-bit execution mode, headers and [Forwarding Pointers](fwd-ptr.html)
> are exactly the same size and therefore the header bytes can be reused to
> store a forwarding pointer during GC. In 64-bit execution mode, the forwarding
> pointers require 8 bytes, and therefore 4 bytes remain unused when storing
> a header.
>
> However, note that MaPLe currently only supports 64-bit execution mode, and
> it's unclear whether or not the 32-bit execution mode will be supported in the
> future. So, currently, we are stuck in a bit of a limbo, where the header is
> 8 bytes but we hesitate to use the upper 4 bytes... just in case.