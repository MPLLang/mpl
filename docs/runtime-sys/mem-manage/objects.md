---
layout: page
title: Heap Objects
parent: Memory Management
---

# Heap Objects

Heap-allocated memory objects, a.k.a. **heap objects** or just **objects**, are
laid out carefully and tagged with GC metadata.

Every object has two components: a payload, and metadata. The payload is where
the actual program data is stored. The metadata records information about the
contents and size of the payload, as well as other info used by the garbage
collector. The metadata usually consists of just a [header](header.md), which is
8 bytes. (Exception: sequence objects have 24 bytes of metadata. See below.)

Pointers to objects always point to the beginning of the payload. These
pointers are known as "object pointers", and are identified by the C type
[`objptr`](https://github.com/MPLLang/mpl/blob/5bcbcc1883a12edf0dd0f29bc46e65c2cc2ff65e/runtime/gc/objptr.h#L13) throughout `runtime/gc/`. 

![General layout of any MPL heap object]({{site.baseurl}}/assets/heap-obj-model.png){:width="50%"}

## Object Types

The MPL runtime system uses three types of objects: normal objects,
sequence objects, and stack objects. (See `NORMAL_TAG`, `SEQUENCE_TAG`, and
`STACK_TAG` in the definition of the enum [`GC_objectTypeTag`](https://github.com/MPLLang/mpl/blob/5bcbcc1883a12edf0dd0f29bc46e65c2cc2ff65e/runtime/gc/object.h#L19-L26).)
The type of every object is recorded in its [header](header.html).

![Normal, sequence, and stack objects]({{site.baseurl}}/assets/heap-obj-types.png){:width="85%"}

* **Normal objects** are flexible: they may contain any number of fields, fixed at
the time of allocation (and known at compile time). Each field contains
either an object pointer (`objptr`) pointing to another object, or it contains
non-pointer data (e.g., integer, boolean, floating point number).
Typically, normal objects correspond to tuples and/or records in the source program.
For example, a source-level tuple of type `Int32.int * Real64.real` will be compiled
into a normal object with a payload of 12 (=4+8) bytes. The metadata for a
normal object is just a [header](header.html).

* **Sequence objects** correspond to the source-level types `array` and `vector`,
containing any number of elements (fixed at the time of allocation and not
necessarily known at compile time). Each element is essentially just the payload
of a normal object, and all elements within a single sequence have exactly the
same layout. The metadata of a sequence is larger: in addition to an 8-byte
[header](header.html), there is also an 8-byte length and an 8-byte "counter"
(which may be used by the garbage collector to record how much of the sequence
has been scanned.)

* **Stack objects** are call-stacks, manipulated by the compiled code, used
to record local variables and arguments for function calls. See [Call-Stacks](stack.html).
The metadata of a stack objct is just a [header](header.html).

{: .note}
Inherited from MLton, there are also **weak** objects, but MPL does not
currently support these. Weak objects have an object type tag
([`WEAK_TAG`](https://github.com/MPLLang/mpl/blob/5bcbcc1883a12edf0dd0f29bc46e65c2cc2ff65e/runtime/gc/object.h#L25)),
but we should never see them. The garbage collector checks for this and will
crash (see [`forwardHHObjptr`](https://github.com/MPLLang/mpl/blob/5bcbcc1883a12edf0dd0f29bc46e65c2cc2ff65e/runtime/gc/hierarchical-heap-collection.c#L1802-L1805))
if it ever comes across a weak object.