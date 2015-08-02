/* Copyright (C) 2015 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file logging.h
 *
 * @author Ram Raghunathan
 *
 * This file provides logging function for runtime logging. By default, the log
 * level is ERROR and the log file is stderr.
 */

#ifndef LOGGER_H_
#define LOGGER_H_

#include <stdio.h>

enum LogLevel {
  L_NONE = 0,
  L_ERROR = 1,
  L_WARNING = 2,
  L_INFO = 3,
  L_DEBUG = 4
};

static const size_t L_MAX_MESSAGE_LENGTH = 8196;

/**
 * Sets the global log level
 *
 * @param level The level to set to
 */
void L_setLevel(enum LogLevel level);

/**
 * Sets the global log file
 *
 * @param file The file object to log to
 */
void L_setFile(FILE* file);

/**
 * This function creates a log message
 *
 * @attention
 * As this function prepends the message with message metadata, the formatted
 * message length cannot exceed MAX_MESSAGE_LENGTH or it will be truncated.
 *
 * @param logPredicate Predicate for whether to log or not
 * @param flush Whether to flush after logging or not
 * @param level The log level of this message
 * @param processor The processor logging this message
 * @param function The function logging this message
 * @param format The format of the message as per 'printf()'
 * @param ... The format arguments as per 'printf()'
 */
void L_log(bool logPredicate,
           bool flush,
           enum LogLevel level,
           size_t processor,
           const char* function,
           const char* format,
           ...)
    __attribute__((format (printf, 6, 7)));

#endif /* LOGGER_H_ */
