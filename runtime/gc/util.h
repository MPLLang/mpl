/* Copyright (C) 2015 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file util.h
 *
 * @author Ram Raghunathan
 *
 * This file defines a utility interface used in the runtime implementation
 */

#ifndef UTIL_H_
#define UTIL_H_

/**
 * This is a convenience function for logging which automatically fetches the
 * processor number and function name
 *
 * @param s The GC_state to use to fetch the processor number
 * @param logPredicate See L_log()
 * @param flush See L_Log()
 * @param level See L_Log()
 * @param ... Both the format and the arguments for the message
 */
#define LOG(s, logPredicate, flush, level, ...)  \
  L_log(logPredicate,                                    \
        flush,                                           \
        level,                                           \
        Proc_processorNumber(s),                         \
        __func__,                                        \
        __VA_ARGS__)

/**
 * Improved die() using the logger
 *
 * @param s The GC_state to use to fetch the processor number. See 'LOG()'.
 * @param ... Both the format and the arguments for the message.
 */
#define DIE(s, ...)                               \
  do {                                            \
    fflush(NULL);                                 \
    LOG(s, true, true, L_ERROR, __VA_ARGS__);     \
    exit(1);                                      \
  } while(false)

#endif /* UTIL_H_ */
