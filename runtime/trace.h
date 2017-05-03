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
  EVENT_FINISH                = 6,

  EVENT_GC_ENTER              = 2,
  EVENT_GC_LEAVE              = 3,

  EVENT_HALT_REQ              = 4,
  EVENT_HALT_WAIT             = 5,

  EVENT_THREAD_COPY           = 7,
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