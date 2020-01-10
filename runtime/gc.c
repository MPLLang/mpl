/* Copyright (C) 2012,2014 Matthew Fluet.
 * Copyright (C) 1999-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#define MLTON_GC_INTERNAL_TYPES
#define MLTON_GC_INTERNAL_FUNCS
#define MLTON_GC_INTERNAL_BASIS
#include "platform.h"

#if ASSERT
#define ARG_USED_FOR_ASSERT
#define LOCAL_USED_FOR_ASSERT
#else
#define ARG_USED_FOR_ASSERT  __attribute__ ((unused))
#define LOCAL_USED_FOR_ASSERT  __attribute__ ((unused))
#endif

#ifdef ENABLE_TRACING
#define ARG_USED_FOR_TRACING
#define LOCAL_USED_FOR_TRACING
#else
#define ARG_USED_FOR_TRACING  __attribute__ ((unused))
#define LOCAL_USED_FOR_TRACING  __attribute__ ((unused))
#endif

#include "gc/virtual-memory.c"
#include "gc/align.c"
#include "gc/read_write.c"

/* RAM_NOTE: Remove if I move to a different TLS model */
/* used to look up per-processor state */
extern C_Pthread_Key_t gcstate_key;

#include "gc/assign.c"
#include "gc/atomic.c"
#include "gc/call-stack.c"
#include "gc/chunk.c"
#include "gc/controls.c"
#include "gc/copy-thread.c"
#include "gc/current.c"
#include "gc/termination.c"
#include "gc/local-scope.c"
#include "gc/done.c"
#include "gc/deferred-promote.c"
#include "gc/enter_leave.c"
#include "gc/foreach.c"
#include "gc/forward.c"
#include "gc/frame.c"
#include "gc/processor.c"
#include "gc/garbage-collection.c"
#include "gc/gc_state.c"
#include "gc/handler.c"
#include "gc/heap.c"
#include "gc/hierarchical-heap.c"
#include "gc/hierarchical-heap-collection.c"
#include "gc/init-world.c"
#include "gc/init.c"
#include "gc/int-inf.c"
#include "gc/invariant.c"
#include "gc/local-heap.c"
#include "gc/logger.c"
#include "gc/model.c"
#include "gc/new-object.c"
#include "gc/object-size.c"
#include "gc/object.c"
#include "gc/objptr.c"
#include "gc/pack.c"
#include "gc/parallel.c"
#include "gc/pointer.c"
#include "gc/profiling.c"
#include "gc/remembered-set.c"
#include "gc/rusage.c"
#include "gc/sequence-allocate.c"
#include "gc/sequence.c"
#include "gc/share.c"
#include "gc/signals.c"
#include "gc/size.c"
#include "gc/sources.c"
#include "gc/stack.c"
#include "gc/statistics.c"
#include "gc/switch-thread.c"
#include "gc/thread.c"
#include "gc/weak.c"
#include "gc/world.c"
