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

static objptr Assignable_findLockedTrueReplicaSlow(
  GC_state s, objptr o, struct HM_HierarchicalHeap **phh,
  bool for_reading, bool keep_intermediate_levels_locked);

static pointer Assignable_findLockedTrueReplica(
  GC_state s, objptr o, struct HM_HierarchicalHeap **phh,
  bool for_reading, bool keep_intermediate_levels_locked) {
    if (phh) {
        *phh = NULL;
    }

    assert(isObjptr(o));

    /* Objects in the global heap are not supposed to have forwarding pointers
     * outside of collection periods. */
    if (isObjptrInGlobalHeap(s, o)) {
        goto fast_path;
    }

    assertObjptrInHH(o);

    /* If the object has no forwarding pointer, we try to be smart before
     * calling the general locking code. */
    if (!hasFwdPtr(objptrToPointer(o, NULL))) {
        struct HM_HierarchicalHeap *hhCurrent;
        struct HM_ObjptrInfo info;

        assert (getThreadCurrent(s)->hierarchicalHeap != BOGUS_OBJPTR);
        hhCurrent =
            (struct HM_HierarchicalHeap *)
            objptrToPointer(getThreadCurrent(s)->hierarchicalHeap,
                            NULL);
        HM_getObjptrInfo(s, o, &info);

        /* We do not have to lock anything if the object lives either at a leaf
         * (since no one can promote it concurrently), or in the root (since no
         * further promotion can occur). */

        if (info.level == hhCurrent->level) {
            /* at a leaf */
            goto fast_path;
        }

        if (info.level == 0) {
            /* at the root */
            goto fast_path;
        }
    }

    /** TODO implement a fast-path for when the object is at the root without a
     * forwarding pointer. */

    o = Assignable_findLockedTrueReplicaSlow(s, o, phh,
                                             for_reading,
                                             keep_intermediate_levels_locked);

fast_path:
    assert (!hasFwdPtr(objptrToPointer(o, NULL)));
    return objptrToPointer(o, NULL);
}

static objptr Assignable_findLockedTrueReplicaSlow(
  GC_state s, objptr o, struct HM_HierarchicalHeap **phh,
  bool for_reading, bool keep_intermediate_levels_locked) {
    assert (s);
    assert (o);
    assertObjptrInHH(o);

    struct HM_HierarchicalHeap *hh, *new_hh;
    objptr o_orig = o;

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
    while (hasFwdPtr(objptrToPointer(o, NULL))) {
        /* ... follow the forwarding pointer, unlock the current heap (if
         * lock_intermediate is false), and lock the HH where the forwarding
         * field points to. Since non-NULL forwarding pointers cannot be changed
         * by other threads, doing things in this order works. */

        o = getFwdPtr(objptrToPointer(o, NULL));
        assertObjptrInHH(o);

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

    /* Store the current HH. */
    if (phh)
        *phh = hh;

    if (o_orig != o) {
      Trace2(EVENT_PROMOTED_WRITE,
             (EventInt)(void *)objptrToPointer(o_orig, NULL),
             (EventInt)(void *)objptrToPointer(o, NULL));
    }

    /* Return the true replica, which is locked. */
    return o;
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

int Assignable_isMaster (GC_state s, objptr o) {
    return !hasFwdPtr(objptrToPointer(o, NULL));
}

objptr Assignable_get (GC_state s, objptr src, Int64 index) {
    // struct HM_HierarchicalHeap *hh;
    pointer src_repl, res;

    // src_repl = Assignable_findLockedTrueReplicaReader(s, src, &hh);
    src_repl = objptrToPointer(src, NULL);
    res = *((pointer *)src_repl + index);
    // Assignable_unlockReplicaReader(s, hh);

    return pointerToObjptr(res, NULL);
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
  assert(isObjptr(dst));
  pointer dstp = objptrToPointer(dst, NULL);
  objptr* field = (objptr*)dstp + index;

  /* Go ahead and perform the write */
  *field = src;

  /* If src does not reference an object, then no need to check for
   * down-pointers. */
  if (!isObjptr(src))
    return;

  HM_chunkList dstList = HM_getLevelHeadPathCompress(HM_getChunkOf(dstp));

  /* Objects in the global heap are handled specially during collections. */
  bool dstInGlobalHeap = (0 == HM_getChunkListLevel(dstList));
  if (dstInGlobalHeap)
    return;

  pointer srcp = objptrToPointer(src, NULL);
  HM_chunkList srcList = HM_getLevelHeadPathCompress(HM_getChunkOf(srcp));

  /* This creates a down pointer; must be remembered. */
  if (dstList->level < srcList->level) {
    assert(getHierarchicalHeapCurrent(s) != NULL);
    struct HM_HierarchicalHeap* hh = getHierarchicalHeapCurrent(s);
    Word32 level = srcList->level;
    if (NULL == HM_HH_LEVEL(hh, level)) {
      HM_HH_LEVEL(hh, level) = HM_newChunkList(hh, level);
    }
    HM_rememberAtLevel(HM_HH_LEVEL(hh, level), dst, field, src);
  }
}
