/* Copyright (C) 2016 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file spinlock.c
 *
 * @author Ram Raghunathan
 *
 * This file implements the spinlock interface defined in spinlock.c
 */

#include "spinlock.h"

/***********/
/* Defines */
/***********/

#define SPINLOCK_UNLOCKED SPINLOCK_INITIALIZER
#define SPINLOCK_LOCKED (~SPINLOCK_INITIALIZER)

/************************/
/* Function Definitions */
/************************/

void spinlock_init(spinlock_t* lock) {
  *lock = SPINLOCK_INITIALIZER;
}

void spinlock_lock(spinlock_t* lock) {
  do {
  } while (!spinlock_trylock(lock));
}


bool spinlock_trylock(spinlock_t* lock) {
  return (__sync_bool_compare_and_swap (lock,
                                        SPINLOCK_UNLOCKED,
                                        SPINLOCK_LOCKED));
}

void spinlock_unlock(spinlock_t* lock) {
  *lock = SPINLOCK_UNLOCKED;
  __sync_synchronize();
}
