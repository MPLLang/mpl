#include "level.h"

static inline bool inRangeOfHH(struct HM_HierarchicalHeap *hh, Word32 level) {
  return (hh->stealLevel + 1 <= level && level <= hh->level);
}

struct HM_HierarchicalHeap *HM_foreachHHUp(GC_state s,
                                           struct HM_HierarchicalHeap *bottom,
                                           Word32 minLevel,
                                           hhFunction function,
                                           void *arg) {
  assert (bottom);

  LOG(LM_HIERARCHICAL_HEAP, LL_INFO,
      "traversing from %p [%u, %u] up to %u",
      (void *)bottom,
      bottom->stealLevel + 1,
      bottom->level,
      minLevel);

  if (minLevel > bottom->level) {
    return NULL;
  }

  for (;
       bottom && !inRangeOfHH(bottom, minLevel);
       bottom = HM_HH_objptrToStruct(s, bottom->parentHH)) {
    assert (minLevel <= bottom->stealLevel);
    function(bottom, arg);
  }

  assert (bottom);

  function(bottom, arg);

  return bottom;
}

struct hhDownFun_arg {
  Word32 minLevel;
  Word32 prevLevel;
  hhFunction clientFun;
  void *clientArg;
};

void hhDownFun(struct HM_HierarchicalHeap *, void *);
void hhDownFun(struct HM_HierarchicalHeap *hh, void *p) {
    assert (hh);

    struct hhDownFun_arg *arg = (struct hhDownFun_arg *)p;

    if (inRangeOfHH(hh, arg->minLevel)) {
      arg->clientFun(hh, arg->clientArg);
    } else {
      arg->prevLevel = hh->stealLevel + 1;
    }
}

void HM_foreachHHDown(GC_state s,
                      struct HM_HierarchicalHeap *bottom, Word32 minLevel,
                      hhFunction fun, void *arg) {
    assert (bottom);

    /* Since we cannot actually walk HHs from top to bottom, we have to use an
     * O(N^2) algorithm with two nested loops. In practice N is probably going
     * to be small enough that this will not matter. */

    struct hhDownFun_arg internalArg = {
      .minLevel = minLevel,
      .clientFun = fun,
      .clientArg = arg,
    };

    while (bottom != HM_foreachHHUp(s,
                                    bottom,
                                    internalArg.minLevel,
                                    hhDownFun,
                                    &internalArg)) {
      assert (internalArg.minLevel < internalArg.prevLevel);
      internalArg.minLevel = internalArg.prevLevel;
    }
}

pointer HM_followForwardPointerUntilNullOrBelowLevel(GC_state s,
                                                     pointer p,
                                                     Word32 minLevel) {
    assert (s);
    assert (p);

    struct HM_ObjptrInfo info;
    LOCAL_USED_FOR_ASSERT Word32 prevLevel = minLevel;
    LOCAL_USED_FOR_ASSERT pointer orig = p;

#if ASSERT
    HM_getObjptrInfo(s, pointerToObjptr(p, NULL), &info);
    assert (info.level != CHUNK_INVALID_LEVEL);
    if (info.level < minLevel) {
        DIE("pointer %p is already at level %d < %d\n",
            (void *)p,
            info.level,
            minLevel);
    }
    prevLevel = info.level;
#endif  /* ASSERT */

    while (hasFwdPtr(p)) {
        objptr next = getFwdPtr(p);
        HM_getObjptrInfo(s, next, &info);

        /* Either we are in to-space, or the level has to decrease strictly. */
        assert (HM_isObjptrInToSpace(s, next) || prevLevel >= info.level);

        if (info.level < minLevel) {
            break;
        }
        p = objptrToPointer(next, NULL);
    }

    LOG(LM_HH_PROMOTION, LL_DEBUG,
        "followed fwd pointers from %p to %p",
        (void *)orig, (void *)p);

    return p;
}

bool HM_objptrIsAboveHH(GC_state s,
                        pointer p,
                        struct HM_HierarchicalHeap *hh) {
  struct HM_ObjptrInfo info;
  GC_header header;
  GC_objectTypeTag tag;
  uint16_t bytesNonObjptrs;
  uint16_t numObjptrs;

  HM_getObjptrInfo(s, pointerToObjptr(p, NULL), &info);
  header = getHeader(p);
  splitHeader(s, header, &tag, NULL, &bytesNonObjptrs, &numObjptrs);

  if (info.hh == hh)
    return true;

  for (struct HM_HierarchicalHeap* cursor = info.hh;
       cursor != NULL;
       cursor = HM_HH_objptrToStruct(s, cursor->parentHH)) {
    if (cursor == hh) {
      LOG(LM_HH_COLLECTION, LL_INFO,
          "Pointer %p (h: %lx, t: %s BNO: %"PRIu16" NO: %"PRIu16")"
          " resides in hh %p which is a descendant of collecting hh %p",
          ((void *)p),
          header,
          objectTypeTagToString(tag),
          bytesNonObjptrs,
          numObjptrs,
          (void *)info.hh,
          (void *)hh);
      return false;
    }
  }

  for (struct HM_HierarchicalHeap* cursor = hh;
       cursor != NULL;
       cursor = HM_HH_objptrToStruct(s, cursor->parentHH)) {
    if (cursor == info.hh) {
      return true;
    }
  }

  LOG(LM_HH_COLLECTION, LL_INFO,
      "Pointer %p (h: %lx, t: %s BNO: %"PRIu16" NO: %"PRIu16")"
      " resides in hh %p which is not an ancestor of collecting hh %p",
      (void *)p,
      header,
      objectTypeTagToString(tag),
      bytesNonObjptrs,
      numObjptrs,
      (void *)info.hh,
      (void *)hh);
  return false;
}
