Comments:
This implements LC + CC + Concurrent Execution. No Policy yet.

1) Make storage of concurrent data specific to depth = 1. We don't need to allocate cp for all heaps, etc.

2) there is some cleanup required in concurrent-collection.c

3) Handle the case where the computation finishes before collection properly.

4) Also, the assertion in GC_HH_mergeThreads thread.c trips because incounter is not decreased. This is okay for non debug compilations but is something that still needs fixing.

5) Figure out why forwarding pointers is needed at all while in CC. Probably, there is some bug in local collection which leaves some field pointing to the forwarding pointer of the object it copies or there is some missed downptr which makes LC unable to change all such fields. Either way there is some problem in the current system which leaves these objects whose only use is to have a header that forwards to some other objects. This is true even with CC turned off.

6) Potential Problem: We may possibly collect fwdPointer objects without changing from where they are accessed. This might break collection but hasn't so far.


Several of these issues have been addressed and this will be deleted in the next Commit:

1) This went out of the window because we have enabled internal heap collection. The optimisation done now is only storing these things till logP depth. The cp space is still there for further depths.
2) some has been done
3) It is mostly okay except for an assert in GC_HH_mergeThreads which trips in debug mode. Not a problem in normal mode
4) --
5) this is still unknown
6) This has been taken care of by preserving all the intermediate forwarding pointers.
