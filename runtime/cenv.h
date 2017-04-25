/* Copyright (C) 2012 Matthew Fluet.
 * Copyright (C) 1999-2009 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _MLTON_CENV_H_
#define _MLTON_CENV_H_

/********************************/
/* GNU C Library Feature Macros */
/********************************/
/* C11 Features */
#define _ISOC11_SOURCE

/* POSIX 2008 Features */
/*
 * Specify _BSD, _SVID and _DEFAULT_SOURCE for glibc >= 2.20 and glibc < 2.20
 * compatibility
 */
#define _BSD_SOURCE
#define _SVID_SOURCE
#define _DEFAULT_SOURCE

/* Only enable _POSIX_C_SOURCE on platforms that don't have broken
 * system headers.
 */
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

/* Various compile-time sanity checks */
#define COMPILE_TIME_ASSERT(name, x) \
        typedef int _COMPILE_TIME_ASSERT___##name[(x) ? 1 : -1]
COMPILE_TIME_ASSERT(CHAR_BIT__is_eight, CHAR_BIT == 8);
COMPILE_TIME_ASSERT(sizeof_float__is_four, sizeof(float) == 4);
COMPILE_TIME_ASSERT(sizeof_double__is_eight, sizeof(double) == 8);

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

/**
 * This platform switch sets platform-specific includes for fenv.h, inttypes.h,
 * stdint.h, and more
 */
#if (defined (__GNUC__))
#if ((__GNUC__ >= 4) && (__GNUC_MINOR__ >= 8))
#include "platform/atomics-gcc-gte48.h"
#else
#include "platform/atomics-gcc-lt48.h"
#endif
#endif

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
#elif (defined (__s390__))
#include "platform/s390.h"
#elif (defined (__sparc__))
#include "platform/sparc.h"
#elif (defined (__i386__))
#include "platform/x86.h"
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
COMPILE_TIME_ASSERT(sizeof_uintptr_t__is__sizeof_voidStar,
                    sizeof(uintptr_t) == sizeof(void*));
COMPILE_TIME_ASSERT(sizeof_uintptr_t__is__sizeof_size_t,
                    sizeof(uintptr_t) == sizeof(size_t));
COMPILE_TIME_ASSERT(sizeof_uintptr_t__is__sizeof_ptrdiff_t,
                    sizeof(uintptr_t) == sizeof(ptrdiff_t));
COMPILE_TIME_ASSERT(sizeof_voidStar__is__pointer_bits,
                    sizeof(void*)*CHAR_BIT == POINTER_BITS);
COMPILE_TIME_ASSERT(address_bits__lte__pointer_bits,
                    ADDRESS_BITS <= POINTER_BITS);

/* Defines EXTERNAL, PRIVATE, PUBLIC for MLton FFI systems */
#include "export.h"

#endif /* _MLTON_CENV_H_ */
