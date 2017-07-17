/* Copyright (C) 2016 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file log.h
 *
 * @author Ram Raghunathan
 *
 * This file provides logging function for runtime logging.
 */

#ifndef LOG_H_
#define LOG_H_

#include <stdio.h>

enum LogLevel {
  LL_FORCE = 0,
  LL_ASSERT = 1,
  LL_ERROR = 2,
  LL_WARNING = 3,
  LL_INFO = 4,
  LL_DEBUG = 5,
  LL_DEBUGMORE = 6
};

#define L_MAX_LOG_LEVEL_LENGTH 128
#define L_MAX_MESSAGE_LENGTH 8196


/**
 * Sets the global log file
 *
 * @param file The file object to log to
 */
void L_setFile(FILE* file);

/**
 * Checks if logging is enabled for the given message and log levels.
 *
 * @param messageLevel The level of the message to be logged.
 * @param logLevel The level of the logger system for this message.
 *
 * @return TRUE if this message's level is enabled according to 'logLevel',
 * FALSE otherwise
 */
bool L_levelEnabled(enum LogLevel messageLevel, enum LogLevel logLevel);

/**
 * This function creates a log message
 *
 * @attention
 * As this function prepends the message with message metadata, the formatted
 * message length cannot exceed MAX_MESSAGE_LENGTH or it will be truncated.
 *
 * @param flush Whether to flush after logging or not
 * @param level The log level of this message
 * @param processor The processor logging this message
 * @param function The function logging this message
 * @param format The format of the message as per 'printf()'
 * @param ... The format arguments as per 'printf()'
 */
void L_log(bool flush,
           enum LogLevel level,
           size_t processor,
           const char* function,
           const char* format,
           ...)
    __attribute__((format (printf, 5, 6)));

#endif /* LOGGER_H_ */
