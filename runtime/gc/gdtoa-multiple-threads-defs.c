
pthread_mutex_t GLOBAL_GDTOA_LOCK_0 = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t GLOBAL_GDTOA_LOCK_1 = PTHREAD_MUTEX_INITIALIZER;

void ACQUIRE_DTOA_LOCK(int n) {
  if (n == 0) {
    pthread_mutex_lock(&GLOBAL_GDTOA_LOCK_0);
  }
  else if (n == 1) {
    pthread_mutex_lock(&GLOBAL_GDTOA_LOCK_1);
  }
  else {
    DIE("ACQUIRE_DTOA_LOCK: bad lock identifier");
  }
}

void FREE_DTOA_LOCK(int n) {
  if (n == 0) {
    pthread_mutex_unlock(&GLOBAL_GDTOA_LOCK_0);
  }
  else if (n == 1) {
    pthread_mutex_unlock(&GLOBAL_GDTOA_LOCK_1);
  }
  else {
    DIE("ACQUIRE_DTOA_LOCK: bad lock identifier");
  }
}

unsigned int dtoa_get_threadno(void) {
  GC_state s = pthread_getspecific(gcstate_key);
  if (NULL != s) {
    return (unsigned int)(s->procNumber);
  }
  return 0;
}