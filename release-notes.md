With this version,
we lift the disentanglement restriction from MPL
and extend it to support all fork-join programs, no holds barred.
<!--  -->
MPL supports entangled programs by treating entanglement as an object-level property:
it distinguishes entangled objects from disentangled objects and manages them in-place,
without moving them.

MPL dynamically distinguishes entangled objects and disentangled objects.
It does so with the help of the read barrier (see `runtime/gc/assign.c`), which
intercepts mutable reads and checks if they create entanglement.
If the read is entangled, the barrier pins the `entanglement region` of the entangled object,
instructing the collector not to move any object of the region
(performed by the function `manage_entangled`, see `runtime/gc/decheck.c`).
The write barrier accounts for inter-heap pointers created as a result of mutable updates by adding
them to the remembered set of the target heaps (see `Assignable_writeBarrier` in `runtime/gc/assign.c`).
The collection algorithm is a hybrid algorithm, which keeps the pinned entangled objects in place
and relocates disentangled objects to compact them (see function `markAndAdd` and `HM_HHC_collectLocal` in `hierarchical-heap-collection.c`).

