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

static uint32_t *synch_depths = SYNCH_DEPTHS_BASE;

void decheckInit(GC_state s) {
    if (mmap(SYNCH_DEPTHS_BASE, SYNCH_DEPTHS_LEN, PROT_WRITE,
            MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, 0, 0) == MAP_FAILED) {
        perror("mmap error");
        exit(-1);
    }
    memset(synch_depths, 0, MAX_PATHS * sizeof(uint32_t));

    GC_thread thread = getThreadCurrent(s);
    thread->decheckState.internal.path = 1;
    thread->decheckState.internal.depth = 0;
    synch_depths[1] = 0;
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
}

void GC_HH_decheckSetTid(GC_state s, uint64_t bits) {
    decheck_tid_t tid;
    tid.bits = bits;

    GC_thread thread = getThreadCurrent(s);
    thread->decheckState = tid;
    uint32_t p = norm_path(tid);
    synch_depths[p] = dag_depth(tid);
}

void GC_HH_decheckJoin(GC_state s, uint64_t left, uint64_t right) {
    decheck_tid_t t1;
    t1.bits = left;
    decheck_tid_t t2;
    t2.bits = right;

    GC_thread thread = getThreadCurrent(s);
    uint32_t p1 = norm_path(t1);
    uint32_t p2 = norm_path(t2);
    unsigned int td = tree_depth(t1) - 1;
    unsigned int dd = MAX(synch_depths[p1], synch_depths[p2]) + 1;
    decheck_tid_t tid;
    tid.internal.path = t1.internal.path | (1 << td);
    tid.internal.depth = (dd << 5) + td;
    thread->decheckState = tid;
    synch_depths[norm_path(tid)] = dd;
}

static bool isOrdered(decheck_tid_t t1, decheck_tid_t t2) {
    uint32_t p1 = norm_path(t1);
    uint32_t p2 = norm_path(t2);
    if (p1 == p2)
        return true;
    int x = p1 ^ p2;
    int lca_mask = (x & -x) - 1;
    uint32_t lca_path = (p1 & lca_mask) + lca_mask + 1;
    assert(lca_path < MAX_PATHS);
    return p1 == lca_path || dag_depth(t1) <= synch_depths[lca_path];
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
    if (!isOrdered(allocator, tid)) {
        printf("Entanglement detected: object at %p\n", (void *) ptr);
        printf("Allocator tree depth: %d\n", tree_depth(allocator));
        printf("Allocator path: 0x%x\n", allocator.internal.path);
        printf("Allocator dag depth: %d\n", dag_depth(allocator));
        printf("Reader tree depth: %d\n", tree_depth(tid));
        printf("Reader path: 0x%x\n", tid.internal.path);
        printf("Reader dag depth: %d\n", dag_depth(tid));
        exit(-1);
    }
}
