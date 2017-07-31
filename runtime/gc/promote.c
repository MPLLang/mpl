/* Copyright (C) 2017 Adrien Guatto.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

pointer HM_Promote(GC_state s,
                   struct HM_ChunkInfo *dst_chunk,
                   pointer src) {
    dst_chunk = HM_getChunkHeadChunk(dst_chunk);
    struct HM_ChunkInfo *src_chunk =
        HM_getObjptrLevelHeadChunk(s, pointerToObjptr(src, s->heap->start));
    struct HM_HierarchicalHeap *dst_hh =
        dst_chunk->split.levelHead.containingHH;

    LOG(LM_HH_PROMOTION, LL_INFO,
        "Promoting src %p to chunk list %p",
        (void *)src, (void *)dst_chunk);

    Trace2(EVENT_PROMOTION_ENTER, (EventInt)src, (EventInt)dst_chunk);

    /* AG_NOTE is this needed? */
    getStackCurrent(s)->used = sizeofGCStateCurrentStackUsed (s);
    getThreadCurrent(s)->exnStack = s->exnStack;

    assert (!dst_hh->newLevelList);

    struct ForwardHHObjptrArgs forwardHHObjptrArgs = {
        .hh = dst_hh,
        .minLevel = dst_chunk->level + 1,
        .maxLevel = src_chunk->level,
        .tgtLevel = dst_chunk->level,
        .tgtFromChunkList = HM_getChunkHeadChunk(dst_chunk),
        .bytesCopied = 0,
        .objectsCopied = 0,
        .stacksCopied = 0
    };

    LOG(LM_HH_PROMOTION, LL_DEBUG,
        "promoting %p to chunk %p:\n"
        "  scope is %u -> %u\n",
        (void *)src,
        (void *)dst_chunk,
        forwardHHObjptrArgs.minLevel,
        forwardHHObjptrArgs.maxLevel);

    objptr srcobj = pointerToObjptr(src, s->heap->start);

    LOG(LM_HH_PROMOTION, LL_DEBUG, "START src copy");

    forwardHHObjptr(s, &srcobj, &forwardHHObjptrArgs);

    LOG(LM_HH_PROMOTION, LL_DEBUG, "START copy loop");

    HM_forwardHHObjptrsInLevelList(
        s,
        &dst_hh->newLevelList,
        trueObjptrPredicate,
        NULL,
        &forwardHHObjptrArgs,
        true);

    /* We should only ever have one level. */
    assert(!HM_getChunkInfo(dst_hh->newLevelList)->split.levelHead.nextHead);

    /* We need to ensure some invariants.

       1/ Merge the level list.

       2/ Reset the to-space level list.

       3/ Reset the cached pointer from the from-space chunk list to the
       to-space chunk list.

       4/ Update locallyCollectibleSize if we have been promoting to a locally
       collectible level.
    */

    size_t lcs_dst_level =
      HM_getChunkInfo(dst_hh->newLevelList)->split.levelHead.size;
    HM_mergeLevelList(&dst_hh->levelList, dst_hh->newLevelList, dst_hh, true);

    dst_hh->newLevelList = NULL;

    assert (forwardHHObjptrArgs.tgtFromChunkList);
    assert (HM_getChunkInfo(forwardHHObjptrArgs.tgtFromChunkList)->
            split.levelHead.toChunkList);
    HM_getChunkInfo(forwardHHObjptrArgs.tgtFromChunkList)->
        split.levelHead.toChunkList = NULL;

    struct HM_HierarchicalHeap *current_hh = getHierarchicalHeapCurrent(s);
    if (dst_chunk->level >= HM_HH_getLowestPrivateLevel(s, current_hh)) {
      assert (dst_hh == current_hh);
      dst_hh->locallyCollectibleSize += lcs_dst_level;
    }

    assertInvariants(s, dst_hh, LIVE);

    /* Exit. */

    Trace0(EVENT_PROMOTION_LEAVE);

    pointer res = objptrToPointer(srcobj, s->heap->start);

    LOG(LM_HH_PROMOTION, LL_DEBUG,
        "Promoted src %p to replica %p",
        (void *)src, (void *)res);

    Trace2(EVENT_PROMOTION, (EventInt)(void *)src, (EventInt)(void *)res);

    return res;
}
