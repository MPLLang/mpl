/* Copyright (C) 2017 Ram Raghunathan.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

/**
 * @file valgrind.c
 *
 * @author Ram Raghunathan
 *
 * @brief
 * This file provides macros for Valgrind's memcheck. To enable them, define the
 * macro VALGRIND.
 */

#ifndef VALGRIND_H_
#define VALGRIND_H_

#ifdef VALGRIND
#include <valgrind/memcheck.h>
#else
#define VALGRIND_CREATE_MEMPOOL(pool, rzB, is_zeroed) do { } while(0)
#define VALGRIND_MAKE_MEM_NOACCESS(ptr, size) do { } while(0)
#define VALGRIND_MEMPOOL_ALLOC(pool, addr, size) do { } while(0)
#define VALGRIND_MEMPOOL_FREE(pool, addr) do { } while(0)
#define VALGRIND_MAKE_MEM_UNDEFINED(ptr, size) do { } while(0)
#endif

#endif /* VALGRIND_H_ */
