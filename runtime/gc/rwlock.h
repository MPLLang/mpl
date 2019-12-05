/* Copyright (C) 1999-2017 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef RWLOCK_H_
#define RWLOCK_H_

#include <stdatomic.h>

typedef atomic_char lock_t;

void lock_init(lock_t *lock);
void lock_lock(lock_t *lock);
void lock_unlock(lock_t *lock);
void lock_lock_explicit(lock_t *lock, bool check);
void lock_unlock_explicit(lock_t *lock, bool check);
bool lock_is_locked(const lock_t *lock);
bool lock_is_locked_by_me(const lock_t *lock);

typedef struct {
    lock_t reader_lock;
    lock_t writer_lock;
    _Atomic uint16_t readers_in_section;
} __attribute__((packed)) rwlock_t;

COMPILE_TIME_ASSERT(rwlock_size,
                    sizeof(rwlock_t) == 4);

void rwlock_init(rwlock_t *lock);

void rwlock_reader_lock(GC_state s, rwlock_t *lock);
void rwlock_reader_unlock(GC_state s, rwlock_t *lock);

void rwlock_writer_lock(GC_state s, rwlock_t *lock);
void rwlock_writer_unlock(GC_state s, rwlock_t *lock);

typedef enum {
  RWLOCK_STATUS_NOBODY,
  RWLOCK_STATUS_READERS,
  RWLOCK_STATUS_WRITER
} lock_status;

lock_status rwlock_status(const rwlock_t *lock);
bool rwlock_is_locked_by_me_for_writing(const rwlock_t *lock);

#endif /* RWLOCK_H_ */
