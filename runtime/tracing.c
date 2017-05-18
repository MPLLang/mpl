/* Copyright (C) 2017 Adrien Guatto.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#include <sys/time.h>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "tracing.h"

struct TracingContext *TracingNewContext(const char *filename,
                                         size_t bufferCapacity,
                                         uint32_t procNumber) {
  struct TracingContext *ctx;

  if ((ctx = malloc(sizeof *ctx)) == NULL) {
    fprintf(stderr, "Tracing: could not allocate context\n");
    exit(1);
  }

  if ((ctx->buffer = calloc(bufferCapacity, sizeof *ctx->buffer)) == NULL) {
    fprintf(stderr, "Tracing: could not allocate buffer\n");
    exit(1);
  }

  if ((ctx->file = fopen(filename, "wb")) == NULL) {
    fprintf(stderr, "Tracing: could not open file %s\n", filename);
    exit(1);
  }

  ctx->id = procNumber;
  ctx->capacity = bufferCapacity;
  ctx->index = 0;

  Trace_(ctx, EVENT_INIT, 0, 0, 0);

  return ctx;
}

void TracingCloseAndFreeContext(struct TracingContext **ctx) {
  if (*ctx == NULL)
    return;

  /* Mark termination in the log file. */
  Trace_(*ctx, EVENT_FINISH, 0, 0, 0);

  TracingFlushBuffer(*ctx);

  fclose((*ctx)->file);
  free((*ctx)->buffer);
  free(*ctx);
  *ctx = NULL;
}

void TracingFlushBuffer(struct TracingContext *ctx) {
  assert(ctx);
  assert(ctx->file);
  assert(ctx->index <= ctx->capacity);

  size_t nitems = ctx->index;

  if (fwrite(ctx->buffer, sizeof *ctx->buffer, nitems, ctx->file) < nitems) {
    fprintf(stderr, "Tracing: could not write to file\n");
    exit(1);
  }

  ctx->index = 0;
}

static inline void
TracingGetTimespec(struct timespec *ts)
{
#if defined(__APPLE__)
  struct timeval tv;

  gettimeofday(&tv, NULL);
  ts->tv_sec = tv.tv_sec;
  ts->tv_nsec = 1000 * tv.tv_usec;
#elif defined(CLOCK_MONOTONIC_RAW)
  clock_gettime(CLOCK_MONOTONIC_RAW, ts);
#else
  clock_gettime(CLOCK_MONOTONIC, ts);
#endif
}

void Trace_(struct TracingContext *ctx, int kind,
            EventInt arg1, EventInt arg2, EventInt arg3) {
  if (!ctx)
    return;

  assert(ctx->index < ctx->capacity);

  struct Event ev;
  ev.kind = kind;
  ev.argptr = ctx->id;
  TracingGetTimespec(&ev.ts);
  ev.arg1 = arg1;
  ev.arg2 = arg2;
  ev.arg3 = arg3;

  ctx->buffer[ctx->index++] = ev;
  if (ctx->index == ctx->capacity)
    TracingFlushBuffer(ctx);
}
