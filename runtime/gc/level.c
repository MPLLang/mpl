#include "level.h"

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
       cursor = cursor->parentHH) {
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
       cursor = cursor->parentHH) {
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
