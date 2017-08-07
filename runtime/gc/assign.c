/* Copyright (C) 1999-2017 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static pointer Assignable_findLockedTrueReplica(
  GC_state s, objptr o, struct HM_HierarchicalHeap **phh,
  bool for_reading, bool keep_intermediate_levels_locked);

static pointer Assignable_findLockedTrueReplica(
  GC_state s, objptr o, struct HM_HierarchicalHeap **phh,
  bool for_reading, bool keep_intermediate_levels_locked) {
    assert (s);
    assert (o);
    struct HM_HierarchicalHeap *hh, *new_hh;
    objptr o_orig = o;

    if (phh)
        *phh = NULL;

    /* Do not do anything for objects living in the global heap. */
    if (!HM_HH_objptrInHierarchicalHeap(s, o)) {
        return objptrToPointer(o, s->heap->start);
    }

    hh = HM_getObjptrHH(s, o);

    LOG(LM_HH_PROMOTION, LL_INFO,
        "replica locking %p for %s",
        (void *)hh,
        for_reading ? "reading" : "writing");
    if (for_reading) {
        lockReaderHH(hh);
    } else {
        lockWriterHH(hh);
    }

    /* While we have not reached the true replica... */
    while (hasFwdPtr(objptrToPointer(o, s->heap->start))) {
        /* ... follow the forwarding pointer, unlock the current heap (if
         * lock_intermediate is false), and lock the HH where the forwarding
         * field points to. Since non-NULL forwarding pointers cannot be changed
         * by other threads, doing things in this order works. */

        o = getFwdPtr(objptrToPointer(o, s->heap->start));
        assert (HM_HH_objptrInHierarchicalHeap(s, o));

        new_hh = HM_getObjptrHH(s, o);

        /* If keep_intermediate_levels_locked is true, we should lock all
         * intermediate HHs between the current one and the new one. */

        if (keep_intermediate_levels_locked) {
          /* Lock all HHs between hh (excluded) and new_hh (included). */
          while (hh != new_hh) {
            hh = HM_HH_objptrToStruct(s, hh->parentHH);
            assert (hh);

            LOG(LM_HH_PROMOTION, LL_INFO,
                "replica locking %p on my way to %p for %s",
                (void *)hh,
                (void *)new_hh,
                for_reading ? "reading" : "writing");

            if (for_reading) {
              lockReaderHH(hh);
            } else {
              lockWriterHH(hh);
            }
          }
        } else {
          /* Unlock the old hh. */
          if (for_reading) {
            unlockReaderHH(hh);
          } else {
            unlockWriterHH(hh);
          }
          hh = new_hh;
          /* Lock the new HH. */
          if (for_reading) {
            lockReaderHH(hh);
          } else {
            lockWriterHH(hh);
          }
        }

        assert (hh == new_hh);
    }

    /* Store the current lock. */
    if (phh)
        *phh = hh;

    if (o_orig != o) {
      Trace2(EVENT_PROMOTED_WRITE,
             (EventInt)(void *)objptrToPointer(o_orig, s->heap->start),
             (EventInt)(void *)objptrToPointer(o, s->heap->start));
    }

    /* Return the true replica, which is locked. */
    return objptrToPointer(o, s->heap->start);
}

pointer Assignable_findLockedTrueReplicaReader(
  GC_state s, objptr o, struct HM_HierarchicalHeap **phh) {
    return Assignable_findLockedTrueReplica(s, o, phh, true, false);
}

pointer Assignable_findLockedTrueReplicaWriter(
  GC_state s, objptr o, struct HM_HierarchicalHeap **phh) {
    return Assignable_findLockedTrueReplica(s, o, phh, false, false);
}

void Assignable_unlockReplicaReader(
  GC_state s, struct HM_HierarchicalHeap *hh) {
    if (hh) {
        unlockReaderHH(hh);
    }
}

void Assignable_unlockReplicaWriter(
  GC_state s, struct HM_HierarchicalHeap *hh) {
    if (hh) {
        unlockWriterHH(hh);
    }
}

objptr Assignable_get (GC_state s, objptr src, Int64 index) {
    struct HM_HierarchicalHeap *hh;
    pointer src_repl, res;

    src_repl = Assignable_findLockedTrueReplicaReader(s, src, &hh);
    res = *((pointer *)src_repl + index);
    Assignable_unlockReplicaReader(s, hh);

    return pointerToObjptr(res, s->heap->start);
}

struct hhLockUnlock_args {
  bool for_locking;
  struct HM_HierarchicalHeap *prev_hh;
};

void hhLockUnlock(struct HM_HierarchicalHeap *hh, void *arg);
void hhLockUnlock(struct HM_HierarchicalHeap *hh, void *arg) {
    assert (hh);

    struct hhLockUnlock_args *args = (struct hhLockUnlock_args *)arg;

    LOG(LM_HH_PROMOTION, LL_INFO,
        "range %slocking %p [%u, %u]",
        args->for_locking ? "" : "un",
        (void *)hh,
        hh->stealLevel + 1,
        hh->level);

    if (args->for_locking) {
        lockWriterHH(hh);
    } else {
        unlockWriterHH(hh);
    }
}

void Assignable_set(GC_state s, objptr dst, Int64 index, objptr src) {
    struct HM_ObjptrInfo src_info, dst_info;
    pointer dst_repl;

    LOG(LM_HH_PROMOTION, LL_INFO,
        "Starting Assignable_set(dst = %p, src = %p)",
        (void *)objptrToPointer(dst, s->heap->start),
        (void *)objptrToPointer(src, s->heap->start));

    /* Assignments to objects in the global heap are not locked. Writing a
     * pointer in the hierarchical heap to the global heap is only permitted
     * when writing to the deque. */
    if (!HM_HH_objptrInHierarchicalHeap(s, dst)) {
        if (HM_HH_objptrInHierarchicalHeap(s, src)) {
            assert (BOGUS_OBJPTR != s->wsQueue);
            pointer queuep = objptrToPointer(s->wsQueue, s->heap->start);
            objptr afterlastp = pointerToObjptr(getArrayAfterLastp(s, queuep),
                                                s->heap->start);
            if (dst < s->wsQueue || dst >= afterlastp) {
                LOG(LM_HH_PROMOTION, LL_DEBUGMORE,
                    "Writing %p from HH to %p in global heap",
                    (void *)dst,
                    (void *)src);
            } else {
                LOG(LM_HH_PROMOTION, LL_DEBUG,
                    "Writing %p to the deque at %p",
                    (void *)objptrToPointer(src, s->heap->start),
                    (void *)queuep);
            }
        }
        LOG(LM_HH_PROMOTION, LL_DEBUG,
            "Writing %p to the global heap at %p",
            (void *)objptrToPointer(src, s->heap->start),
            (void *)objptrToPointer(dst, s->heap->start));
        /* Perform the write. */
        *((pointer *)objptrToPointer(dst, s->heap->start)
          + index) = objptrToPointer(src, s->heap->start);
        goto end;
    }

    /* Pointer from the hierarhical heap to the global heap cannot trigger
     * promotions. Just find the true replica. */
    if (!HM_HH_objptrInHierarchicalHeap(s, src)) {
        struct HM_HierarchicalHeap *hh;
        dst_repl = Assignable_findLockedTrueReplicaWriter(s, dst, &hh);
        *((pointer *)dst_repl + index) = objptrToPointer(src, s->heap->start);
        Assignable_unlockReplicaWriter(s, hh);
        goto end;
    }

    HM_getObjptrInfo(s, src, &src_info);
    HM_getObjptrInfo(s, dst, &dst_info);

    LOG(LM_HH_PROMOTION, LL_INFO,
        "Locking from level %u of %p to level %u below %p",
        HM_getChunkHeadChunk(HM_getChunkInfo(src_info.chunkList))->level,
        (void *)objptrToPointer(src, s->heap->start),
        dst_info.level + 1,
        (void *)objptrToPointer(dst, s->heap->start));

    /* Take the writer lock on every level between src (included) and dst
     * (excluded). */
    struct hhLockUnlock_args args = {
      .for_locking = true,
      .prev_hh = NULL,
    };
    HM_foreachHHUp(s,
                   src_info.hh,
                   dst_info.level + 1,
                   hhLockUnlock,
                   &args);

    /* Find the true replica of the destination, locking intermediate levels
     * along the way. */

    if (rwlock_is_locked_by_me_for_writing(&dst_info.hh->lock)) {
      /* We first unlock the HH of dst, which might have been locked above if it
       * is also the HH of src. Without this precaution.
       * Assignable_findLockedTrueReplica() might try to lock it again and thus
       * deadlock. */
      LOG(LM_HH_PROMOTION, LL_INFO,
          "Unlocking %p to avoid deadlock",
          (void *)dst_info.hh);
      unlockWriterHH(dst_info.hh);
    }

    dst_repl = Assignable_findLockedTrueReplica(s, dst, NULL, false, true);

    LOG(LM_HH_PROMOTION, LL_INFO,
        "Found true replica %p of %p",
        (void *)dst_repl,
        (void *)objptrToPointer(dst, s->heap->start));

    HM_getObjptrInfo(s, pointerToObjptr(dst_repl, s->heap->start), &dst_info);

    /* Promote memory reachable from src to dst_repl when needed to avoid
     * entanglement. */
    pointer src_repl = objptrToPointer(src, s->heap->start);
    if (dst_info.level < src_info.level) {
        src_repl = HM_Promote(s, HM_getChunkInfo(dst_info.chunkList), src_repl);
    }

    /* Perform the write. */
    *((pointer *)dst_repl + index) = src_repl;

    /* Unlock all the levels between dst_repl and src. */
    LOG(LM_HH_PROMOTION, LL_INFO,
        "Unlocking from level %u of %p to level %u of %p",
        dst_info.level,
        (void *)objptrToPointer(dst, s->heap->start),
        HM_getChunkHeadChunk(HM_getChunkInfo(src_info.chunkList))->level,
        (void *)objptrToPointer(src, s->heap->start));
    args.for_locking = false;
    args.prev_hh = NULL;
    HM_foreachHHDown(s,
                     src_info.hh,
                     dst_info.level,
                     hhLockUnlock,
                     &args);


end:
    LOG(LM_HH_PROMOTION, LL_INFO,
        "Ending Assignable_set(dst = %p, src = %p)",
        (void *)objptrToPointer(dst, s->heap->start),
        (void *)objptrToPointer(src, s->heap->start));
}
