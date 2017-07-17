/* Copyright (C) 2015,2016 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file log.c
 *
 * @author Ram Raghunathan
 *
 * This file implements the log interface defined in log.h
 */

#include "log.h"

#include <stdio.h>

/********************/
/* Global Constants */
/********************/
static const char* LogLevelToString[] = {
  "FORCE",
  "ASSERT",
  "ERROR",
  "WARN",
  "INFO",
  "DEBUG",
  "DEBUGM"
};

static const int LEVEL_FIELD_WIDTH = sizeof("ASSERT") - 1;

/********************/
/* Global Variables */
/********************/
static FILE* logFile = NULL;

/************************/
/* Function Definitions */
/************************/
void L_setFile(FILE* file) {
  logFile = file;
}

bool L_levelEnabled(enum LogLevel messageLevel, enum LogLevel logLevel) {
  return (messageLevel <= logLevel);
}

void L_log(bool flush,
           enum LogLevel level,
           size_t processor,
           const char* function,
           const char* format,
           ...) {
  char formattedMessage[L_MAX_MESSAGE_LENGTH];
  va_list substitutions;
  va_start(substitutions, format);
  vsnprintf(formattedMessage, sizeof(formattedMessage), format, substitutions);
  va_end(substitutions);

  fprintf(logFile,
          "%-*s [P%02zd|%s]: %s\n",
          LEVEL_FIELD_WIDTH,
          LogLevelToString[level],
          processor,
          function,
          formattedMessage);

  if (flush) {
    fflush(logFile);
  }
}
