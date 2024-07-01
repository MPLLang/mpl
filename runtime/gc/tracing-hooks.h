/* Copyright (C) 2024 Sam Westrick
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef TRACING_HOOKS_H_
#define TRACING_HOOKS_H_


#if (defined (MLTON_GC_INTERNAL_BASIS))

PRIVATE void GC_Trace_schedIdleEnter(GC_state s);
PRIVATE void GC_Trace_schedIdleLeave(GC_state s);

PRIVATE void GC_Trace_schedWorkEnter(GC_state s);
PRIVATE void GC_Trace_schedWorkLeave(GC_state s);

PRIVATE void GC_Trace_schedSleepEnter(GC_state s);
PRIVATE void GC_Trace_schedSleepLeave(GC_state s);

PRIVATE void GC_Trace_schedSpawn(GC_state s);
PRIVATE void GC_Trace_schedJoin(GC_state s);
PRIVATE void GC_Trace_schedJoinFast(GC_state s);

#endif // defined(MLTON_GC_INTERNAL_BASIS)


#endif
