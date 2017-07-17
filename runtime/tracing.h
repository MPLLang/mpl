/* Copyright (C) 2017 Adrien Guatto.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef TRACING_H
#define TRACING_H

#include <stdint.h>

#include "trace.h"

/* A structure holding the information required to record tracing
 * messages. Messages are buffered into memory and periodically flushed to disk
 * when the buffer is full. */
struct TracingContext {
  struct Event *buffer;
  size_t id;
  size_t index;
  size_t capacity;
  FILE *file;
};

/* Allocates a new tracing context and open its backing file. */
struct TracingContext *TracingNewContext(const char *filename,
                                         size_t bufferCapacity,
                                         uint32_t procNumber);

/* Close a trace file and free the corresponding context. The buffer is
 * flushed. */
void TracingCloseAndFreeContext(struct TracingContext **ctx);

/* Flush recent events to the backing file. This function is automatically
 * called by Trace() when the trace buffer is full, so there should be no need
 * to call it manually. */
void TracingFlushBuffer(struct TracingContext *ctx);

/* Add a new log event to the tracing context. */
void Trace_(struct TracingContext *ctx, int kind,
            EventInt arg1, EventInt arg2, EventInt arg3);

#ifdef ENABLE_TRACING
#define Trace(...) Trace_((s)->trace, __VA_ARGS__)
#define WITH_GCSTATE(code)                              \
  do {                                                  \
    GC_state s = pthread_getspecific(gcstate_key);      \
    code;                                               \
  } while (0);
#else
#define Trace(...) ((void)0)
#define WITH_GCSTATE(code) ((void)0)
#endif

#define Trace0(k)               Trace(k,  0,  0,  0)
#define Trace1(k, a0)           Trace(k, a0,  0,  0)
#define Trace2(k, a0, a1)       Trace(k, a0, a1,  0)
#define Trace3(k, a0, a1, a2)   Trace(k, a0, a1, a2)

#endif  /* TRACING_H */
