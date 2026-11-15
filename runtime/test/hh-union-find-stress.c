/* Copyright (C) 2026 Kostiantyn Dementiev.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

/* Stress test: heap queries racing with union-find linking (issue #227).
 *
 * HM_getLevelHeadPathCompress may run on one processor while another
 * processor links the representative of the same set into a new
 * representative (as linkInto in gc/hierarchical-heap.c does at a join)
 * and then queries the same set. Each round builds:
 *
 *   chunkA -> n[0] -> n[1] -> ... -> n[LEN-1] -> t1        t0
 *                                    ^
 *                                  chunkB
 *
 * Thread A queries chunkA. At a varying moment, thread B links t1 into t0
 * and then queries chunkB. Either answer (t1 or t0) is a correct result
 * for thread A, but afterwards t0 must still be the root and every node
 * must reach t0 without a cycle. A query that does not return within ten
 * seconds, or a crash, is also a failure.
 *
 * Build the runtime first (e.g. with `make` at the top level), then run:
 *   make -C runtime/test check
 */

#define MLTON_GC_INTERNAL_TYPES
#include "platform.h"

#include <pthread.h>
#include <sched.h>
#include <signal.h>
#include <stdatomic.h>
#include <time.h>

HM_HierarchicalHeap HM_getLevelHeadPathCompress(HM_chunk chunk);

/* The runtime library refers to these symbols, which a compiled program
 * normally provides. They are not used here. */
C_Pthread_Key_t gcstate_key;
GC_state MLton_gcState(void) { return NULL; }

#define LEN 1000
#define DEFAULT_ROUNDS 20000
#ifndef MAX_DELAY
#define MAX_DELAY 200000
#endif

static struct HM_UnionFindNode n[LEN], t0, t1;
static struct HM_HierarchicalHeap hn[LEN], ht0, ht1;
static struct HM_chunk chunkA, chunkB;

/* Round handshake: main publishes round r in `go`; each thread publishes r
 * in its `done` flag when it has finished round r. */
static atomic_ulong go;
static atomic_ulong doneA;
static atomic_ulong doneB;
static atomic_ulong currentRound;
static unsigned long delay;
static HM_HierarchicalHeap resultA, resultB;

static void waitFor(atomic_ulong *flag, unsigned long r) {
  while (atomic_load_explicit(flag, memory_order_acquire) < r)
    sched_yield();
}

static void initNode(HM_UnionFindNode u, HM_HierarchicalHeap h,
                     HM_UnionFindNode rep) {
  u->representative = rep;
  u->payload = h;
  u->dependant1 = NULL;
  u->dependant2 = NULL;
  h->ufNode = u;
}

static void initChunk(HM_chunk c, HM_UnionFindNode levelHead) {
  memset(c, 0, sizeof(*c));
  c->levelHead = levelHead;
  c->magic = CHUNK_MAGIC;
}

static void resetRound(void) {
  for (int i = 0; i < LEN; i++)
    initNode(&n[i], &hn[i], (i + 1 < LEN) ? &n[i + 1] : &t1);
  initNode(&t1, &ht1, NULL);
  initNode(&t0, &ht0, NULL);
  initChunk(&chunkA, &n[0]);
  initChunk(&chunkB, &n[LEN - 1]);
}

static void *threadA(__attribute__ ((unused)) void *arg) {
  for (unsigned long r = 1; ; r++) {
    waitFor(&go, r);
    resultA = HM_getLevelHeadPathCompress(&chunkA);
    atomic_store_explicit(&doneA, r, memory_order_release);
  }
  return NULL;
}

static void *threadB(__attribute__ ((unused)) void *arg) {
  for (unsigned long r = 1; ; r++) {
    waitFor(&go, r);
    for (volatile unsigned long i = 0; i < delay; i++) { }
    /* the link performed by linkInto(s, left = t0's heap, right = t1's heap) */
    t1.representative = &t0;
    resultB = HM_getLevelHeadPathCompress(&chunkB);
    atomic_store_explicit(&doneB, r, memory_order_release);
  }
  return NULL;
}

static void onSignal(int sig) {
  char msg[160];
  int len = snprintf(msg, sizeof(msg),
    "FAIL: signal %d in round %lu (crash or failed assertion in a "
    "query%s)\n", sig,
    atomic_load_explicit(&currentRound, memory_order_relaxed),
    (t0.representative == &t1) ? "; t0 now points to t1, a cycle" : "");
  if (write(STDERR_FILENO, msg, len) < 0) { }
  _exit(1);
}

static double now(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec + ts.tv_nsec * 1e-9;
}

/* Every node must reach t0 within LEN+2 steps, and t0 must be the root. */
static const char *checkStructure(void) {
  if (t0.representative != NULL)
    return "t0 is no longer a root (cycle through t0)";
  if (t1.representative != &t0)
    return "t1 does not point to t0";
  for (int i = 0; i < LEN; i++) {
    HM_UnionFindNode cursor = &n[i];
    int steps = 0;
    while (cursor->representative != NULL && steps <= LEN + 2) {
      cursor = cursor->representative;
      steps++;
    }
    if (cursor != &t0)
      return "a node does not reach t0";
  }
  return NULL;
}

int main(int argc, char **argv) {
  unsigned long rounds = (argc > 1) ? strtoul(argv[1], NULL, 10) : DEFAULT_ROUNDS;
  struct sigaction sa;
  memset(&sa, 0, sizeof(sa));
  sa.sa_handler = onSignal;
  sigaction(SIGSEGV, &sa, NULL);
  sigaction(SIGBUS, &sa, NULL);
  sigaction(SIGABRT, &sa, NULL);

  pthread_t a, b;
  pthread_create(&a, NULL, threadA, NULL);
  pthread_create(&b, NULL, threadB, NULL);

  unsigned long staleA = 0;
  unsigned long seed = 12345;
  for (unsigned long r = 1; r <= rounds; r++) {
    resetRound();
    seed = seed * 6364136223846793005UL + 1442695040888963407UL;
    delay = (seed >> 33) % MAX_DELAY;
    atomic_store_explicit(&currentRound, r, memory_order_relaxed);
    atomic_store_explicit(&go, r, memory_order_release);

    double deadline = now() + 10.0;
    while (atomic_load_explicit(&doneA, memory_order_acquire) < r
           || atomic_load_explicit(&doneB, memory_order_acquire) < r) {
      sched_yield();
      if (now() > deadline) {
        fprintf(stderr, "FAIL: a query did not return in round %lu "
                "(cycle in the union-find structure)\n", r);
        _exit(1);
      }
    }

    const char *err = checkStructure();
    if (err != NULL) {
      fprintf(stderr, "FAIL: %s in round %lu\n", err, r);
      return 1;
    }
    if (resultB != &ht0) {
      fprintf(stderr, "FAIL: query after the link returned a stale heap "
              "in round %lu\n", r);
      return 1;
    }
    if (resultA != &ht0 && resultA != &ht1) {
      fprintf(stderr, "FAIL: query returned a heap that was never a "
              "representative in round %lu\n", r);
      return 1;
    }
    if (resultA == &ht1) staleA++;
  }

  printf("OK: %lu rounds, query A ran before the link in %lu of them\n",
         rounds, staleA);
  return 0;
}
