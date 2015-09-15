/* Copyright (C) 2015 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file logger.c
 *
 * @author Ram Raghunathan
 *
 * This file implements the logger interface defined in logger.h
 */

#include "logger.h"

#include <stdio.h>

/********************/
/* Global Constants */
/********************/
static const char* LogLevelToString[] = {
  "NONE",
  "ERROR",
  "WARN",
  "INFO",
  "DEBUG"
};

static const int LEVEL_FIELD_WIDTH = sizeof("ERROR") - 1;

/********************/
/* Global Variables */
/********************/
static enum LogLevel logLevel = L_ERROR;
static FILE* logFile = NULL;

/************************/
/* Function Definitions */
/************************/
void L_setLevel(enum LogLevel level) {
  logLevel = level;
}

void L_setFile(FILE* file) {
  logFile = file;
}

void L_log(bool flush,
           enum LogLevel level,
           size_t processor,
           const char* function,
           const char* format,
           ...) {
  if (level > logLevel) {
    /* shouldn't log this message */
    return;
  }

  char formattedMessage[L_MAX_MESSAGE_LENGTH];
  va_list substitutions;
  va_start(substitutions, format);
  vsnprintf(formattedMessage, sizeof(formattedMessage), format, substitutions);
  va_end(substitutions);

  fprintf(logFile,
          "%-*s [P%02zd/%s]: %s\n",
          LEVEL_FIELD_WIDTH,
          LogLevelToString[logLevel],
          processor,
          function,
          formattedMessage);

  if (flush) {
    fflush(logFile);
  }
}
