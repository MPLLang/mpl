/* Copyright (C) 2012,2017 Matthew Fluet.
 * Copyright (C) 1999-2009 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _MLTON_CENV_H_
#define _MLTON_CENV_H_

#if (defined (__linux__) || defined(__GNU__))
#define _POSIX_C_SOURCE 200112L
#define _GNU_SOURCE
#endif

/* Use 64 bit FS interface on all platforms */
#define _FILE_OFFSET_BITS 64

/* ASSERT/DEBUG macros for selectively enabling and disabling code */
#ifndef ASSERT
#define ASSERT 0
#define NDEBUG
#endif

#if !defined(__STDC_VERSION__)
  #error "unknown stdc version"
#endif

#if __STDC_VERSION__ < 201112L
  #error "Requires C11 or newer to use <stdatomic.h>."
#endif

#ifdef __STDC_NO_ATOMICS__
  #error "Compiler does not support <stdatomic.h>"
#endif

/****************/
/* Header Files */
/****************/
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <float.h>
#include <iso646.h>
#include <limits.h>
#include <math.h>
#include <pthread.h>
#include <signal.h>
#include <stdarg.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <gmp.h>

#ifndef STATIC_ASSERT
  #if __STDC_VERSION__ >= 202311L
    #define STATIC_ASSERT(expr, msg) static_assert(expr, msg)
  #elif __STDC_VERSION__ >= 201112L
    #include <assert.h>
    #define STATIC_ASSERT(expr, msg) _Static_assert(expr, msg)
  #endif
#endif

/* Various compile-time sanity checks */
STATIC_ASSERT(CHAR_BIT == 8, "CHAR_BIT is eight");
STATIC_ASSERT(sizeof(float) == 4, "sizeof(float) is four");
STATIC_ASSERT(sizeof(double) == 8, "sizeof(double) is eight");

/**********************************/
/* Platform-Specific Header Files */
/**********************************/
#if (defined (__APPLE_CC__))
#define __Darwin__
#endif

#define set_cpu_affinity(num)                                           \
  do {                                                                  \
    _Pragma ("message \"set_cpu_affinity() not implemented for this platform!\""); \
  } while (0)

#if (defined (_AIX))
#include "platform/aix.h"
#elif (defined (__CYGWIN__))
#include "platform/cygwin.h"
#elif (defined (__Darwin__))
#include "platform/darwin.h"
#elif (defined (__FreeBSD__) || defined(__FreeBSD_kernel__))
#include "platform/freebsd.h"
#elif (defined (__hpux__))
#include "platform/hpux.h"
#elif (defined (__GNU__))
#include "platform/hurd.h"
#elif (defined (__linux__))
#undef set_cpu_affinity
#include "platform/linux.h"
#elif (defined (__MINGW32__))
#include "platform/mingw.h"
#elif (defined (__NetBSD__))
#include "platform/netbsd.h"
#elif (defined (__OpenBSD__))
#include "platform/openbsd.h"
#elif (defined (__sun__))
#include "platform/solaris.h"
#elif (defined (__wasi__))
#include "platform/wasi.h"
#else
#error unknown platform os
#endif

#if (defined (__alpha__))
#include "platform/alpha.h"
#elif (defined (__x86_64__))
#include "platform/amd64.h"
#elif (defined (__arm__))
#include "platform/arm.h"
#elif (defined (__aarch64__))
#include "platform/arm64.h"
#elif (defined (__hppa__))
#include "platform/hppa.h"
#elif (defined (__ia64__))
#include "platform/ia64.h"
#elif (defined (__m68k__))
#include "platform/m68k.h"
#elif (defined (__mips__))
#include "platform/mips.h"
#elif (defined (__powerpc64__))
#include "platform/powerpc64.h"
#elif (defined (__ppc__)) || (defined (__powerpc__))
#include "platform/powerpc.h"
#elif (defined (__riscv))
#include "platform/riscv.h"
#elif (defined (__s390__))
#include "platform/s390.h"
#elif (defined (__sparc__))
#include "platform/sparc.h"
#elif (defined (__i386__))
#include "platform/x86.h"
#elif (defined (__loongarch64))
#include "platform/loongarch64.h"
#elif (defined (__wasm32))
#include "platform/wasm32.h"
#else
#error unknown platform arch
#endif

/* Calculate width of a pointer */
#ifndef POINTER_BITS
#if UINTPTR_MAX == UINT32_MAX
#define POINTER_BITS 32
#elif UINTPTR_MAX == UINT64_MAX
#define POINTER_BITS 64
#else
#error Platform did not set POINTER_BITS and could not guess it.
#endif
#endif

#ifndef ADDRESS_BITS
#define ADDRESS_BITS POINTER_BITS
#endif

/* More compile-time sanity checks */
STATIC_ASSERT(sizeof(uintptr_t) == sizeof(void*),
              "sizeof(uintptr_t) equals sizeof(void *)");
STATIC_ASSERT(sizeof(uintptr_t) == sizeof(size_t),
              "sizeof(uintptr_t) equals sizeof(size_t)");
STATIC_ASSERT(sizeof(uintptr_t) == sizeof(ptrdiff_t),
              "sizeof(uintptr_t) equals sizeof(ptrdiff_t)");
STATIC_ASSERT(sizeof(void*) * CHAR_BIT == POINTER_BITS,
              "sizeof(void *) * CHAR_BIT equals POINTER_BITS");
STATIC_ASSERT(ADDRESS_BITS <= POINTER_BITS,
              "ADDRESS_BITS is less than or equal to POINTER_BITS");

/* Defines EXTERNAL, PRIVATE, PUBLIC for MLton FFI systems */
#include "export.h"

#endif /* _MLTON_CENV_H_ */
