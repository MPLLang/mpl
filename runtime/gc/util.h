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
 * @param logPredicate See L_log()
 * @param flush See L_Log()
 * @param level See L_Log()
 * @param ... Both the format and the arguments for the message
 */
#define LOG(logPredicate, flush, level, ...)                            \
  do {                                                                  \
    if (logPredicate) {                                                 \
      L_log(flush,                                                      \
            level,                                                      \
            Proc_processorNumber(pthread_getspecific(gcstate_key)),     \
            __func__,                                                   \
            __VA_ARGS__);                                               \
    }                                                                   \
  } while(FALSE)

/**
 * Improved die() using the logger
 *
 * @param ... Both the format and the arguments for the message.
 */
#define DIE(...)                                  \
  do {                                            \
    fflush(NULL);                                 \
    LOG(TRUE, TRUE, L_ERROR, __VA_ARGS__);        \
    exit(1);                                      \
  } while(FALSE)

#endif /* UTIL_H_ */
