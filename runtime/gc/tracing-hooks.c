/* Copyright (C) 2024 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#include "tracing-hooks.h"

#if (defined (MLTON_GC_INTERNAL_BASIS))

void GC_Trace_schedIdleEnter(ARG_USED_FOR_TRACING GC_state s) {
  Trace0(EVENT_SCHED_IDLE_ENTER);
}

void GC_Trace_schedIdleLeave(ARG_USED_FOR_TRACING GC_state s) {
  Trace0(EVENT_SCHED_IDLE_LEAVE);
}

void GC_Trace_schedWorkEnter(ARG_USED_FOR_TRACING GC_state s) {
  Trace0(EVENT_SCHED_WORK_ENTER);
}

void GC_Trace_schedWorkLeave(ARG_USED_FOR_TRACING GC_state s) {
  Trace0(EVENT_SCHED_WORK_LEAVE);
}

void GC_Trace_schedSleepEnter(ARG_USED_FOR_TRACING GC_state s) {
  Trace0(EVENT_SCHED_SLEEP_ENTER);
}

void GC_Trace_schedSleepLeave(ARG_USED_FOR_TRACING GC_state s) {
  Trace0(EVENT_SCHED_SLEEP_LEAVE);
}

void GC_Trace_schedSpawn(ARG_USED_FOR_TRACING GC_state s) {
  Trace0(EVENT_SCHED_SPAWN);
}

void GC_Trace_schedJoin(ARG_USED_FOR_TRACING GC_state s) {
  Trace0(EVENT_SCHED_JOIN);
}

void GC_Trace_schedJoinFast(ARG_USED_FOR_TRACING GC_state s) {
  Trace0(EVENT_SCHED_JOINFAST);
}



#endif
