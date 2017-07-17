/* Copyright (C) 2015,2016 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file log-config.h
 *
 * @author Ram Raghunathan
 *
 * @brief
 * Fine-grained logging configuration.
 */

#ifndef LOGGER_H_
#define LOGGER_H_

enum LogModule {
  LM_ALLOCATION,
  LM_CHUNK,
  LM_CHUNK_POOL,
  LM_DFS_MARK,
  LM_FOREACH,
  LM_GARBAGE_COLLECTION,
  LM_GLOBAL_LOCAL_HEAP,
  LM_GC_STATE,
  LM_HIERARCHICAL_HEAP,
  LM_HH_COLLECTION,
  LM_PARALLEL,
  LM_THREAD,
  NUM_LOG_MODULES
};

#define L_MAX_MODULE_LENGTH 128

extern enum LogLevel L_logLevels[NUM_LOG_MODULES];
extern bool L_flushLog[NUM_LOG_MODULES];

/**
 * These macros are stringification helpers
 */
#define STRFY1(x) #x
#define STRFY(x) STRFY1(x)

/**
 * This is a convenience function for logging which automatically fetches the
 * processor number and function name.
 *
 * @param module The module from enum LogModule that this log message belongs
 * to.
 * @param level The log level from enum LogLevel that this message belongs to.
 * @param ... Both the format and the arguments for the message
 */
#define LOG(module, level, ...)                                         \
  do {                                                                  \
    if (L_levelEnabled(level, L_logLevels[module])) {                   \
      GC_state loggerS = pthread_getspecific(gcstate_key);              \
      L_log(L_flushLog[module],                                         \
            level,                                                      \
            (NULL == loggerS) ? (-1) : (Proc_processorNumber(loggerS)), \
            __func__,                                                   \
            __VA_ARGS__);                                               \
    }                                                                   \
  } while(FALSE)

/**
 * This is a function for checking if a logging level is enabled. This is useful
 * to disable regions of code which are only necessary for a log message.
 *
 * @param module The module from enum LogModule that this log message belongs
 * to.
 * @param level The log level from enum LogLevel that this message belongs to.
 */
#define LOG_ENABLED(module, level) (L_levelEnabled(level, L_logLevels[module]))

/**
 * This is a convenience function for a dying message which automatically
 * fetches the processor number and function name.
 *
 * @param ... Both the format and the arguments for the message.
 */
#define DIE(...)                                                        \
  do {                                                                  \
      fflush(NULL);                                                     \
      GC_state loggerS = pthread_getspecific(gcstate_key);              \
      L_log(TRUE,                                                       \
            LL_ERROR,                                                   \
            (NULL == loggerS) ? (-1) : (Proc_processorNumber(loggerS)), \
            __FILE__ ":" STRFY(__LINE__),                               \
            __VA_ARGS__);                                               \
      abort();                                                          \
  } while(FALSE)

/**
 * This is a convenience function for a logging a message before faulting on an
 * assert.
 *
 * @param test The condition to test
 * @param ... Both the format and the arguments for the message.
 */
#if ASSERT
#define ASSERTPRINT(test, ...)                                          \
  do {                                                                  \
    const bool result = (test);                                         \
    if (!result) {                                                      \
      fflush(NULL);                                                     \
      GC_state loggerS = pthread_getspecific(gcstate_key);              \
      L_log(TRUE,                                                       \
            LL_ASSERT,                                                  \
            (NULL == loggerS) ? (-1) : (Proc_processorNumber(loggerS)), \
            __FILE__ ":" STRFY(__LINE__) " (" #test ")",                \
            __VA_ARGS__);                                               \
      abort();                                                          \
    }                                                                   \
  } while(FALSE)
#else
#define ASSERTPRINT(test, ...) do { } while(FALSE)
#endif

/**
 * This function initializes the log levels according to the argument string
 * provided.
 *
 * @note
 * This function prints to stderr if it detects an error.
 *
 * @param arg The argument string, as provided to the \@MLton option.
 *
 * @return TRUE if the log levels were initialized correctly, FALSE otherwise.
 */
bool initLogLevels(const char* arg);

#endif /* LOGGER_H_ */
