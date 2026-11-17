/* Copyright (C) 2026 Kostiantyn Dementiev.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#define MLTON_GC_INTERNAL_TYPES
#include "platform.h"

#include <errno.h>
#include <pthread.h>
#include <sched.h>
#include <stdatomic.h>
#include <time.h>

void HM_initChunkList(HM_chunkList list);
void HM_appendChunk(HM_chunkList list, HM_chunk chunk);
pointer HM_getChunkStart(HM_chunk chunk);
void CC_stack_init(GC_state s, CC_stack* stack);
bool CC_stack_push(GC_state s, CC_stack* stack, void* datum);
bool CC_stack_try_close(CC_stack* stack, HM_chunkList removed);

C_Pthread_Key_t gcstate_key;
GC_state MLton_gcState(void) { return NULL; }

#define NUM_PROCS 4
#define FILLED_BAG 1
#define TIMEOUT_SECONDS 5.0

typedef union {
  struct HM_chunk chunk;
  uint8_t bytes[256];
} FakeChunk;

static struct GC_state proc0;
static CC_stack stack;
static FakeChunk rootChunk, spareChunk;
static struct HM_chunkList removed;
static atomic_int closeResult = -1;

static int failures = 0;

#define CHECK(cond, ...)                                    \
  do {                                                      \
    if (!(cond)) {                                          \
      fprintf(stderr, "FAIL: " __VA_ARGS__);                \
      fprintf(stderr, "\n");                                \
      failures++;                                           \
    }                                                       \
  } while (0)

static double now(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec + ts.tv_nsec * 1e-9;
}

static HM_chunk initChunk(FakeChunk *c, bool used) {
  memset(c, 0, sizeof(*c));
  c->chunk.magic = CHUNK_MAGIC;
  c->chunk.limit = c->bytes + sizeof(c->bytes);
  c->chunk.frontier = HM_getChunkStart(&c->chunk) + (used ? sizeof(void*) : 0);
  return &c->chunk;
}

static void *collector(__attribute__ ((unused)) void *arg) {
  bool closed = CC_stack_try_close(&stack, &removed);
  atomic_store_explicit(&closeResult, closed ? 1 : 0, memory_order_release);
  return NULL;
}

static void reportHeldLocks(void) {
  for (size_t i = 0; i < stack.numStacks; i++) {
    int rc = pthread_mutex_trylock(&(stack.stacks[i].mutex));
    if (rc == 0)
      pthread_mutex_unlock(&(stack.stacks[i].mutex));
    else
      fprintf(stderr, "  lock of bag %zu is still held (trylock: %s)\n",
              i, rc == EBUSY ? "EBUSY" : strerror(rc));
  }
}

int main(void) {
  static struct GC_state init;
  memset(&init, 0, sizeof(init));
  init.numberOfProcs = NUM_PROCS;
  CC_stack_init(&init, &stack);
  HM_initChunkList(&removed);
  memset(&proc0, 0, sizeof(proc0));
  proc0.procNumber = 0;

  CC_stack_data *filled = &(stack.stacks[FILLED_BAG]);
  pthread_mutex_lock(&(filled->mutex));

  pthread_t c;
  pthread_create(&c, NULL, collector, NULL);

  double deadline = now() + TIMEOUT_SECONDS;
  for (;;) {
    int rc = pthread_mutex_trylock(&(stack.stacks[0].mutex));
    if (rc == EBUSY)
      break;
    if (rc == 0)
      pthread_mutex_unlock(&(stack.stacks[0].mutex));
    sched_yield();
    if (now() > deadline) {
      fprintf(stderr, "FAIL: the collector never took lock 0\n");
      _exit(1);
    }
  }

  HM_appendChunk(&(filled->storage), initChunk(&rootChunk, TRUE));
  pthread_mutex_unlock(&(filled->mutex));

  deadline = now() + TIMEOUT_SECONDS;
  while (atomic_load_explicit(&closeResult, memory_order_acquire) < 0) {
    sched_yield();
    if (now() > deadline) {
      fprintf(stderr, "FAIL: CC_stack_try_close did not return within "
              "%.0f s (deadlock)\n", TIMEOUT_SECONDS);
      reportHeldLocks();
      _exit(1);
    }
  }
  pthread_join(c, NULL);

  CHECK(atomic_load(&closeResult) == 0,
        "CC_stack_try_close closed a non-empty stack");
  CHECK(removed.firstChunk == &rootChunk.chunk,
        "the root pushed into bag %d was not handed back", FILLED_BAG);

  for (size_t i = 0; i < stack.numStacks; i++) {
    int rc = pthread_mutex_trylock(&(stack.stacks[i].mutex));
    CHECK(rc == 0, "lock of bag %zu is still held after the failed close", i);
    if (rc == 0)
      pthread_mutex_unlock(&(stack.stacks[i].mutex));
  }

  HM_appendChunk(&(stack.stacks[0].storage), initChunk(&spareChunk, FALSE));
  void *root = (void*)0x1000;
  bool pushed = CC_stack_push(&proc0, &stack, root);
  CHECK(pushed, "push on processor 0 was refused after the failed close "
        "(bag 0 stayed closed; the root is dropped)");

  HM_initChunkList(&removed);
  int attempts = 0;
  while (!CC_stack_try_close(&stack, &removed) && attempts < 10)
    attempts++;
  CHECK(stack.allClosed, "the stack did not close once every bag was empty");
  if (pushed)
    CHECK(removed.firstChunk == &spareChunk.chunk
          && *(void**)HM_getChunkStart(&spareChunk.chunk) == root,
          "the root pushed on processor 0 was not handed back");

  if (failures > 0)
    return 1;
  printf("OK: failed close released every lock and kept the bags open; "
         "the next close took %d more call(s)\n", attempts + 1);
  return 0;
}
