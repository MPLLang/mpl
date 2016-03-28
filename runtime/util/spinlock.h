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
#define SPINLOCK_INITIALIZER ((spinlock_t)(0))

/**
 * Initializes a spinlock
 *
 * @param lock The lock to initialize.
 */
void spinlock_init(spinlock_t* lock);

/**
 * Locks a spinlock.
 *
 * @note
 * This function blocks until the lock is acquired
 *
 * @param lock The lock to initialize
 */
void spinlock_lock(spinlock_t* lock);

/**
 * Attempts to lock a spinlock without blocking.
 *
 * @param lock The lock to attempt to lock without blocking.
 *
 * @return TRUE if the attempt succeeded, FALSE otherwise.
 */
bool spinlock_trylock(spinlock_t* lock);

/**
 * Unlocks a spinlock.
 *
 * @note
 * The lock is assumed to be locked and owned by the calling thread.
 *
 * @param lock The lock to initialize
 */
void spinlock_unlock(spinlock_t* lock);

#endif /* SPINLOCK_H_ */
