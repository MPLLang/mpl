/* Copyright (C) 2016 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file spinlock.h
 *
 * @author Ram Raghunathan
 *
 * This file provides a simple spinlock.
 */

#ifndef SPINLOCK_H_
#define SPINLOCK_H_

#include <stdint.h>

typedef volatile uint32_t spinlock_t;

/**
 * Macro to initialize a spinlock by simple assignment
 */
#define SPINLOCK_INITIALIZER ((spinlock_t)(~0))

/**
 * Initializes a spinlock
 *
 * @param lock The lock to initialize.
 */
void spinlock_init(spinlock_t* lock);

/**
 * Locks a spinlock with the given value.
 *
 * @note
 * This function blocks until the lock is acquired
 *
 * @param lock The lock to initialize
 * @param value the value to lock the lock with. This value should be unique
 * per-processor and is used for deadlock detection assertions. It cannot be
 * ~0.
 */
void spinlock_lock(spinlock_t* lock, uint32_t value);

/**
 * Attempts to lock a spinlock with the given value without blocking.
 *
 * @param lock The lock to attempt to lock without blocking.
 * @param value the value to lock the lock with. This value should be unique
 * per-processor and is used for deadlock detection assertions. It cannot be
 * ~0.
 *
 * @return TRUE if the attempt succeeded, FALSE otherwise.
 */
bool spinlock_trylock(spinlock_t* lock, uint32_t value);

/**
 * Unlocks a spinlock.
 *
 * @note
 * The lock is assumed to be locked and owned by the calling thread.
 *
 * @param lock The lock to initialize
 */
void spinlock_unlock(spinlock_t* lock);

/**
 * Returns the current value of a lock.
 *
 * @param lock The lock to return the value of
 *
 * @return ~0 if the lock is unlocked, the value of the lock otherwise
 */
uint32_t spinlock_value(spinlock_t* lock);


#endif /* SPINLOCK_H_ */
