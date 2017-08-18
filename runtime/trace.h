/* Copyright (C) 2017 Adrien Guatto.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef TRACE_H
#define TRACE_H

#include <stdint.h>
#include <time.h>

typedef unsigned long long EventInt;

enum EventKind {
  EVENT_NIL                   = 0,
  EVENT_INIT                  = 1,
  EVENT_LAUNCH                = 22,
  EVENT_FINISH                = 6,

  EVENT_RUNTIME_ENTER         = 9,
  EVENT_RUNTIME_LEAVE         = 10,

  EVENT_GC_ENTER              = 2,
  EVENT_GC_LEAVE              = 3,
  EVENT_GC_ABORT              = 11,

  EVENT_HEAP_OCCUPANCY        = 21,

  EVENT_CHUNKP_OCCUPANCY      = 8,
  EVENT_CHUNKP_RATIO          = 12,

  EVENT_THREAD_COPY           = 7,

  EVENT_HALT_REQ              = 4,
  EVENT_HALT_WAIT             = 5,
  EVENT_HALT_ACK              = 20,

  EVENT_LOCK_TAKE_ENTER       = 13,
  EVENT_LOCK_TAKE_LEAVE       = 14,

  EVENT_RWLOCK_R_TAKE         = 15,
  EVENT_RWLOCK_R_RELEASE      = 25,
  EVENT_RWLOCK_W_TAKE         = 26,
  EVENT_RWLOCK_W_RELEASE      = 27,

  EVENT_GSECTION_BEGIN_ENTER  = 16,
  EVENT_GSECTION_BEGIN_LEAVE  = 17,
  EVENT_GSECTION_END_ENTER    = 18,
  EVENT_GSECTION_END_LEAVE    = 19,

  EVENT_ARRAY_ALLOCATE_ENTER  = 23,
  EVENT_ARRAY_ALLOCATE_LEAVE  = 24,

  EVENT_PROMOTION_ENTER       = 28,
  EVENT_PROMOTION_LEAVE       = 29,
  EVENT_PROMOTED_WRITE        = 30,
  EVENT_PROMOTION             = 31,

  EVENT_MERGED_HEAP           = 32,

  EVENT_COPY                  = 33,
};

#define EventKindCount (sizeof EventKindStrings / sizeof *EventKindStrings)

/* An event found in a trace. */
struct Event {
  /* The kind of event. */
  int kind;
  /* The address of the structure the event is relative to. */
  uintptr_t argptr;
  /* The time at which the event occurred; serialized as a pair of
   * integers. */
  struct timespec ts;
  /* First argument, used in some events and not in others. */
  EventInt arg1;
  /* Second argument, used in some events and not in others. */
  EventInt arg2;
  /* Third argument, used in some events and not in others. */
  EventInt arg3;
};

struct TraceFileHeader {
  EventInt version;
};

#define TraceCurrentVersion 0x20170419ULL

#endif  /* TRACE_H */
