/* Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

static inline void *calloc_safe (size_t count, size_t size) {
  void *res;

  res = calloc (count, size);
  if (NULL == res)
    die ("calloc (%"PRIuMAX", %"PRIuMAX") failed.\n",
         (uintmax_t)count, (uintmax_t)size);
  return res;
}

static inline void fclose_safe (FILE* f) {
  int res;

  res = fclose (f);
  if (-1 == res)
    diee ("fclose (_) failed.\n");
  return;
}

static inline FILE *fdopen_safe (int fd, const char *mode) {
  FILE *res;

  res = fdopen (fd, mode);
  if (0 == res)
    diee ("fopen (%d, %s) failed.\n", fd, mode);
  return res;
}

static inline FILE *fopen_safe (const char *fileName, const char *mode) {
  FILE *res;

  res = fopen (fileName, mode);
  if (0 == res)
    diee ("fopen (%s, %s) failed.\n", fileName, mode);
  return res;
}

static inline void fread_safe (void *buf, size_t size, size_t count, FILE *f) {
  size_t res;

  res = fread (buf, size, count, f);
  if (res != count) {
    diee ("fread ("FMTPTR", %"PRIuMAX", %"PRIuMAX", _) failed "
          "(only read %"PRIuMAX"%s).\n",
          (uintptr_t)buf, (uintmax_t)size, (uintmax_t)count, (uintmax_t)res,
          feof (f) ? "; eof" : "");
  }
}

static inline int fseek_safe (FILE *f, long offset, int whence) {
  int res;

  res = fseek (f, offset, whence);
  if (-1 == res)
    diee ("fseek (_, %"PRIuMAX", %"PRIuMAX") failed.\n",
          (uintmax_t)offset, (uintmax_t)whence);
  return res;
}

static inline void fwrite_safe (const void *buf, size_t size, size_t count,
                                FILE *f) {
  size_t res;

  res = fwrite (buf, size, count, f);
  if (res != count)
    diee ("fwrite (_, %"PRIuMAX", %"PRIuMAX", _) failed "
          "(only wrote %"PRIuMAX").\n",
          (uintmax_t)size, (uintmax_t)count, (uintmax_t)res);
}

static inline void *malloc_safe (size_t size) {
  void *res;

  res = malloc (size);
  if (NULL == res)
    die ("malloc (%"PRIuMAX") failed.\n", (uintmax_t)size);
  return res;
}

static inline int mkstemp_safe (char *template) {
  int fd;

  fd = mkstemp (template);
  if (-1 == fd)
    diee ("mkstemp (%s) failed.\n", template);
  return fd;
}

static inline void pthread_mutex_lock_safe (pthread_mutex_t* rwlock) {
  int retVal;
  if (0 != (retVal = pthread_mutex_lock(rwlock))) {
    errno = retVal;
    diee("pthread_rwlock_wrlock() failed with errno %d", errno);
  }
}

static inline void pthread_mutex_unlock_safe (pthread_mutex_t* rwlock) {
  int retVal;
  if (0 != (retVal = pthread_mutex_unlock(rwlock))) {
    errno = retVal;
    diee("pthread_rwlock_unlock() failed with errno %d", errno);
  }
}

static inline void unlink_safe (const char *pathname) {
  int res;

  res = unlink (pathname);
  if (-1 == res)
    diee ("unlink (%s) failed.\n", pathname);
  return;
}
