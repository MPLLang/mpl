/* Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

objptr getThreadCurrentObjptr (GC_state s) {
  return s->currentThread;
}

GC_thread getThreadCurrent (GC_state s) {
  return threadObjptrToStruct(s, getThreadCurrentObjptr(s));
}

objptr getStackCurrentObjptr (GC_state s) {
  GC_thread thread = getThreadCurrent(s);
  return thread->stack;
}

GC_stack getStackCurrent (GC_state s) {
  pointer p = objptrToPointer(getStackCurrentObjptr(s), s->heap->start);
  return (GC_stack)p;
}

objptr getHierarchicalHeapCurrentObjptr (GC_state s) {
  return getThreadCurrent(s)->hierarchicalHeap;
}

struct HM_HierarchicalHeap* getHierarchicalHeapCurrent (GC_state s) {
  objptr hhObjptr = getHierarchicalHeapCurrentObjptr (s);

  if (!isObjptr (hhObjptr)) {
    return NULL;
  }

  return HM_HH_objptrToStruct(s, hhObjptr);
}
