#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include <signal.h>

#include <pthread.h>
#include <sched.h>
#include <semaphore.h>

void Cycler_init();
void Cycler_begin();
void Cycler_terminate();
void Cycler_cleanup();
void Cycler_report();
