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

/************************/
/* Function Definitions */
/************************/

void spinlock_init(spinlock_t* lock) {
  *lock = SPINLOCK_INITIALIZER;
}

void spinlock_lock(spinlock_t* lock, uint32_t value) {
  do {
  } while (!spinlock_trylock(lock, value));
}


bool spinlock_trylock(spinlock_t* lock, uint32_t value) {
  assert(SPINLOCK_UNLOCKED != value);
  assert(*lock != value);
  return (__sync_bool_compare_and_swap(lock,
                                       SPINLOCK_UNLOCKED,
                                       value));
}

void spinlock_unlock(spinlock_t* lock) {
  __sync_synchronize();
  *lock = SPINLOCK_UNLOCKED;
}

uint32_t spinlock_value(spinlock_t* lock) {
  return *lock;
}
