#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include <signal.h>

#include <pthread.h>
#include <sched.h>
#include <semaphore.h>
#include "platform.h"

void Cycler_init(Int64 _p);
void Cycler_begin();
void Cycler_terminate();
void Cycler_cleanup();
void Cycler_report();
