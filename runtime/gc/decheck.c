/* Copyright (C) 2020 Lawrence Wang, Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#define MAX(x, y) ((x) > (y) ? (x) : (y))

#define MAX_FORK_DEPTH 31
#define MAX_PATHS ((unsigned int) 1 << (MAX_FORK_DEPTH))
#define MAX_DEPTH (1 << 27)

#define SYNCH_DEPTHS_BASE ((void *) 0x100000000000)
#define SYNCH_DEPTHS_LEN (sizeof(uint32_t) * (MAX_PATHS))

#if ASSERT
static uint32_t *synch_depths = SYNCH_DEPTHS_BASE;
#endif

void decheckInit(GC_state s) {
#if ASSERT
    if (mmap(SYNCH_DEPTHS_BASE, SYNCH_DEPTHS_LEN, PROT_WRITE,
            MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, 0, 0) == MAP_FAILED) {
        perror("mmap error");
        exit(-1);
    }
    memset(synch_depths, 0, MAX_PATHS * sizeof(uint32_t));
    synch_depths[1] = 0;
#endif

    GC_thread thread = getThreadCurrent(s);
    thread->decheckState.internal.path = 1;
    thread->decheckState.internal.depth = 0;
}

static inline unsigned int tree_depth(decheck_tid_t tid) {
    return tid.internal.depth & 0x1f;
}

static inline unsigned int dag_depth(decheck_tid_t tid) {
    return tid.internal.depth >> 5;
}

static inline uint32_t norm_path(decheck_tid_t tid) {
    unsigned int td = tree_depth(tid);
    return tid.internal.path & ((1 << (td+1)) - 1);
}

static inline void decheckSetSyncDepth(GC_thread thread, uint32_t pathLen, uint32_t syncDepth) {
  if (pathLen >= DECHECK_DEPTHS_LEN) {
    DIE("attempted to set syncDepths[%"PRIu32"] := %"PRIu32, pathLen, syncDepth);
  }
  thread->decheckSyncDepths[pathLen] = syncDepth;
}

static inline uint32_t decheckGetSyncDepth(GC_thread thread, uint32_t pathLen) {
  if (pathLen >= DECHECK_DEPTHS_LEN) {
    DIE("attempted to get syncDepths[%"PRIu32"]", pathLen);
  }
  return thread->decheckSyncDepths[pathLen];
}

/* SAM_NOTE: TODO: implement this in SML and avoid needing to allocate
 * refs just to pass values by destination-passing through the FFI.
 */
void GC_HH_decheckFork(GC_state s, uint64_t *left, uint64_t *right) {
    GC_thread thread = getThreadCurrent(s);
    decheck_tid_t tid = thread->decheckState;
    assert(tid.bits != DECHECK_BOGUS_BITS);
    unsigned int h = tree_depth(tid);
    assert(h < MAX_FORK_DEPTH);

    decheck_tid_t t1;
    t1.internal.path = (tid.internal.path & ~(1 << h)) | (1 << (h+1));
    t1.internal.depth = tid.internal.depth + (1 << 5) + 1;
    *left = t1.bits;

    decheck_tid_t t2;
    t2.internal.path = (tid.internal.path | (1 << h)) | (1 << (h+1));
    t2.internal.depth = tid.internal.depth + (1 << 5) + 1;
    *right = t2.bits;

    assert(tree_depth(t1) == tree_depth(tid)+1);
    assert(tree_depth(t2) == tree_depth(tid)+1);
    assert(dag_depth(t1) == dag_depth(tid)+1);
    assert(dag_depth(t2) == dag_depth(tid)+1);
    assert((norm_path(t1) ^ norm_path(t2)) == (uint32_t)(1 << h));
}

void GC_HH_decheckSetTid(GC_state s, uint64_t bits) {
    decheck_tid_t tid;
    tid.bits = bits;

    GC_thread thread = getThreadCurrent(s);
    thread->decheckState = tid;

#if ASSERT
    synch_depths[norm_path(tid)] = dag_depth(tid);
#endif
    decheckSetSyncDepth(thread, tree_depth(tid), dag_depth(tid));

    assert(decheckGetSyncDepth(thread, tree_depth(tid)) == synch_depths[norm_path(tid)]);
}

uint64_t GC_HH_decheckGetTid(GC_state s, objptr threadp) {
  GC_thread thread = threadObjptrToStruct(s, threadp);
  return thread->decheckState.bits;
}

void GC_HH_decheckJoin(GC_state s, uint64_t left, uint64_t right) {
    decheck_tid_t t1;
    t1.bits = left;
    decheck_tid_t t2;
    t2.bits = right;

    assert(tree_depth(t1) == tree_depth(t2));
    assert(tree_depth(t1) >= 1);

    GC_thread thread = getThreadCurrent(s);
    unsigned int td = tree_depth(t1) - 1;
    unsigned int dd = MAX(dag_depth(t1), dag_depth(t2)) + 1;
    assert(dag_depth(t1) == synch_depths[norm_path(t1)]);
    assert(dag_depth(t2) == synch_depths[norm_path(t2)]);
    decheck_tid_t tid;
    tid.internal.path = t1.internal.path | (1 << td);
    tid.internal.depth = (dd << 5) + td;
    thread->decheckState = tid;

    assert(tree_depth(tid) == tree_depth(t1)-1);

#if ASSERT
    synch_depths[norm_path(tid)] = dd;
#endif
    decheckSetSyncDepth(thread, tree_depth(tid), dd);

    assert(decheckGetSyncDepth(thread, tree_depth(tid)) == synch_depths[norm_path(tid)]);
}

#if ASSERT

static inline int msbPosition(uint32_t x) {
  int i = 0;
  while (x != 1) {
    x = x >> 1;
    i++;
  }
  return i;
}

static inline int lcaLen(uint32_t p1, uint32_t p2) {
  int i = 0;
  int p1len = msbPosition(p1);
  int p2len = msbPosition(p2);
  int mlen = (p1len < p2len) ? p1len : p2len;
  while (i < mlen && ((p1 & 1) == (p2 & 1))) {
    p1 = p1 >> 1;
    p2 = p2 >> 1;
    i++;
  }
  return i;
}

#endif

/* Very fancy bit hack. Finds the index of one set bit. From
 *   https://graphics.stanford.edu/~seander/bithacks.html#IntegerLogDeBruijn
 * Based on info from the paper:
 *   Using de Bruijn Sequences to Index 1 in a Computer Word
 *   by Charles E. Leiserson, Harald Prokof, and Keith H. Randall.
 */
// static const int DeBruijnBitPosition[32] =
// {
//   0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
//   31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
// };
// static inline int bitIndex(uint32_t v) {
//   return DeBruijnBitPosition[(v * 0x077CB531U) >> 27];
// }

// static inline int bitIndex(uint32_t x) {
//   return 31 - __builtin_clz(x);
// }

static inline int bitIndex(uint32_t x) {
  return __builtin_ctz(x);
}

/** The heap depth of the LCA. Recall that heap depths are off-by-one; the
  * "root" of the hierarchy is at depth 1. So, returning the length of the
  * LCA path is the correct heap depth.
  */
static int lcaHeapDepth(GC_thread thread, decheck_tid_t t1)
{
  /** This code is copied from isOrdered... */
  uint32_t p1 = norm_path(t1);
  uint32_t p1mask = (1 << tree_depth(t1)) - 1;
  uint32_t p2 = norm_path(thread->decheckState);
  uint32_t p2mask = (1 << tree_depth(thread->decheckState)) - 1;
  assert(p1 != p2);
  uint32_t shared_mask = p1mask & p2mask;
  uint32_t shared_upper_bit = shared_mask+1;
  uint32_t x = ((p1 ^ p2) & shared_mask) | shared_upper_bit;
  uint32_t lca_bit = x & -x;
  // uint32_t lca_mask = lca_bit-1;
  int llen = bitIndex(lca_bit);
  assert(llen == lcaLen(p1, p2));
  return llen;
}

static bool isOrdered(GC_thread thread, decheck_tid_t t1)
{
  uint32_t p1 = norm_path(t1);
  uint32_t p1mask = (1 << tree_depth(t1)) - 1;
  uint32_t p2 = norm_path(thread->decheckState);
  uint32_t p2mask = (1 << tree_depth(thread->decheckState)) - 1;

  if (p1 == p2)
    return TRUE;

  uint32_t shared_mask = p1mask & p2mask;
  uint32_t shared_upper_bit = shared_mask+1;

  uint32_t x = ((p1 ^ p2) & shared_mask) | shared_upper_bit;
  uint32_t lca_bit = x & -x;
  uint32_t lca_mask = lca_bit-1;
  int llen = bitIndex(lca_bit);
  assert(llen == lcaLen(p1, p2));

  uint32_t lca_path = (p1 & lca_mask) | (lca_mask + 1);
  assert(lca_path < MAX_PATHS);
  assert(decheckGetSyncDepth(thread, llen) == synch_depths[lca_path]);
  return p1 == lca_path || dag_depth(t1) <= decheckGetSyncDepth(thread, llen);
}

void decheckRead(GC_state s, objptr ptr) {
    GC_thread thread = getThreadCurrent(s);
    if (thread == NULL)
        return;
    decheck_tid_t tid = thread->decheckState;
    if (tid.bits == DECHECK_BOGUS_BITS)
        return;
    if (!isObjptr(ptr))
        return;
    HM_chunk chunk = HM_getChunkOf(objptrToPointer(ptr, NULL));
    if (chunk == NULL)
        return;
    decheck_tid_t allocator = chunk->decheckState;
    if (allocator.bits == DECHECK_BOGUS_BITS)
        return;
    if (isOrdered(thread, allocator))
        return;

    /** If we get here, there is entanglement. Next is how to handle it. */

    if (!s->controls->manageEntanglement) {
        printf("Entanglement detected: object at %p\n", (void *) ptr);
        printf("Allocator tree depth: %d\n", tree_depth(allocator));
        printf("Allocator path: 0x%x\n", allocator.internal.path);
        printf("Allocator dag depth: %d\n", dag_depth(allocator));
        printf("Reader tree depth: %d\n", tree_depth(tid));
        printf("Reader path: 0x%x\n", tid.internal.path);
        printf("Reader dag depth: %d\n", dag_depth(tid));
        exit(-1);
    }

    /** set the chunk's disentangled depth. This synchronizes with GC, if there
      * is GC happening by the owner of this chunk.
      */
    int32_t newDD = lcaHeapDepth(thread, allocator);
    assert(newDD >= 1);
    while (TRUE) {
      int32_t oldDD = atomicLoadS32(&(chunk->disentangledDepth));

      /** Negative means it's frozen for GC. Wait until it's unfrozen... */
      while (oldDD < 0) {
        pthread_yield();
        oldDD = atomicLoadS32(&(chunk->disentangledDepth));
      }

      /** And then attempt to update. */
      if (newDD >= oldDD ||
          __sync_bool_compare_and_swap(&(chunk->disentangledDepth), oldDD, newDD))
        break;
    }
}

void GC_HH_copySyncDepthsFromThread(GC_state s, objptr victimThread, uint32_t stealDepth) {
  if (stealDepth > DECHECK_DEPTHS_LEN) {
    DIE("attempted to copy sync depths up to %"PRIu32, stealDepth);
  }

  GC_thread victim = threadObjptrToStruct(s, victimThread);
  GC_thread current = getThreadCurrent(s);

  uint32_t* from = &(victim->decheckSyncDepths[0]);
  uint32_t* to = &(current->decheckSyncDepths[0]);

  /* SAM_NOTE: TODO:
   *
   * I thought it should be possible to just do the first
   * few items, i.e.
   *   memcpy(to, from, stealDepth * sizeof(uint32_t))
   * but this trips the assertions that the thread-local syncDepths match
   * the global syncDepths?? Not sure what's going on.
   *
   * It's safe to copy the whole thing, so I'll do that as a bandaid for
   * now. But really should figure out what's going on here. Some sort of
   * off-by-one error...
   */
  memcpy(to, from, DECHECK_DEPTHS_LEN * sizeof(uint32_t));
}
