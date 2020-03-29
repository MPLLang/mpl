/**
 * @file decheck.c
 * @brief Disentanglement checking
 * @author Lawrence Wang (lawrenc2)
 * @bug No known bugs
 */

#define MAX(x, y) ((x) < (y) ? (x) : (y))

#define MAX_FORK_DEPTH 31
#define MAX_PATHS ((unsigned int) 1 << (MAX_FORK_DEPTH))
#define MAX_DEPTH (1 << 27)

#define SYNCH_DEPTHS_BASE ((void *) 0x100000000000)
#define SYNCH_DEPTHS_LEN (sizeof(uint32_t) * (MAX_PATHS))

static uint32_t synch_depths[MAX_PATHS];

void GC_HH_decheckInit(GC_state s) {
    if (mmap(SYNCH_DEPTHS_BASE, SYNCH_DEPTHS_LEN, PROT_WRITE, 
                MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, 0, 0) == MAP_FAILED) {
        perror("mmap error");
        exit(-1);
    }
    memset(synch_depths, 0, MAX_PATHS * sizeof(uint32_t));

    GC_thread thread = getThreadCurrent(s);
    thread->decheckState.path = 1;
    thread->decheckState.depth = 0;
    synch_depths[1] = 0;
}

static inline unsigned int tree_depth(decheck_tid_t tid) {
    return tid.depth & 0x1f;
}

static inline unsigned int dag_depth(decheck_tid_t tid) {
    return tid.depth >> 5;
}

static inline uint32_t norm_path(decheck_tid_t tid) {
    unsigned int td = tree_depth(tid);
    return tid.path & ((1 << (td+1)) - 1);
}

void GC_HH_decheckFork(GC_state s, decheck_tid_t *left, decheck_tid_t *right) {
    GC_thread thread = getThreadCurrent(s);
    decheck_tid_t tid = thread->decheckState;
    unsigned int h = tree_depth(tid);
    assert(h < MAX_FORK_DEPTH);
    left->path = (tid.path & ~(1 << h)) | (1 << (h+1));
    left->depth = tid.depth + (1 << 5) + 1;
    right->path = tid.path | (1 << h) | (1 << (h+1));
    right->depth = tid.depth + (1 << 5) + 1;
}

void GC_HH_decheckSetTid(GC_state s, decheck_tid_t tid) {
    GC_thread thread = getThreadCurrent(s);
    thread->decheckState = tid;
    uint32_t p = norm_path(tid);
    synch_depths[p] = dag_depth(tid);
}

void GC_HH_decheckJoin(GC_state s, decheck_tid_t t1, decheck_tid_t t2) {
    GC_thread thread = getThreadCurrent(s);
    uint32_t p1 = norm_path(t1);
    uint32_t p2 = norm_path(t2);
    unsigned int td = tree_depth(t1) - 1;
    unsigned int dd = MAX(synch_depths[p1], synch_depths[p2]) + 1; 
    decheck_tid_t tid;
    tid.path = t1.path | (1 << td);
    tid.depth = (dd << 5) + td;
    thread->decheckState = tid;
    synch_depths[norm_path(tid)] = dd;
}

static bool isOrdered(decheck_tid_t t1, decheck_tid_t t2) {
    if (t1.path == t2.path)
        return true;
    int x = t1.path ^ t2.path;
    int lca_mask = (x & -x) - 1;
    uint32_t lca_path = (t1.path & lca_mask) + lca_mask + 1;
    return norm_path(t1) == lca_path || dag_depth(t1) <= synch_depths[lca_path];
}

void decheckRead(GC_state s, objptr *ptr) {
    GC_thread thread = getThreadCurrent(s);
    decheck_tid_t tid = thread->decheckState;
    HM_chunk chunk = HM_getChunkOf((pointer) *ptr);
    decheck_tid_t allocator = chunk->decheckState;
    if (!isOrdered(allocator, tid)) {
        printf("Disentangelent detected: object at %p\n", (void *) *ptr);
        printf("Allocator tree depth: %d\n", tree_depth(allocator));
        printf("Allocator path: 0x%x\n", allocator.path);
        printf("Allocator dag depth: %d\n", dag_depth(allocator));
        printf("Reader tree depth: %d\n", tree_depth(tid));
        printf("Reader path: 0x%x\n", tid.path);
        printf("Reader dag depth: %d\n", dag_depth(tid));
        exit(-1);
    }
}
