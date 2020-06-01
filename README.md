POSSIBLE OPTIMISATIONS ON THIS COMMIT:

1) Figure out why forwrding pointers is needed at all while in CC. Probably, there is some bug in local collection which leaves some field pointing to the forwarding pointer of the object it copies or there is some missed downptr which makes LC unable to change all such fields. Either way there is some problem in the current system which leaves these objects whose only use is to have a header that forwards to some other objects. This is true even with CC turned off.

2) CC needs to preserve all the chunks that have some HH in them. This is because these HHs have to be used for getting level heads of the chunks at depth 1. Somehow, some HHs are ending up in chunks with stopGap = 0. Therefore, CC has no way of figuring which chunks contain HHs. Therefore, CC requires that all the chunks have their levelHead set to the true levelHead and no intermediate hhs with "representative". Currently, this is done in HM_HH_registerCont and therefore sequentially when there is no parallelism. This can possibly be done by CC itself before it starts.

3) Since CC assumes that the hierarchy is dead anyway, there is no reason to preserve startGap = 0 chunks anymore.

4) For simplicity reasons I made the LC SUPER_LOCAL. I am keeping it enabled because this impl. looks like it works and I don't want to change it for this commit. But there is no evidence or reason why the LC needs to be SUPER_LOCAL

5) there is some cleanup required in concurrent-collection.c



This commit now has solved 2, 3, 4. 1 and 5 are pending. CC does not preserve the objects that had fwd pointers. Technically one would have to update the path via which this fwd pointer was reached but that is deemed out of scope of this implementation.