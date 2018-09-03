#define N1 100000
#define N2 10000

#define SETAFFINITY 0

int p = 15; // 0 ~ 15 only when setting affinity
int w = 20;
int W = 1000;

int shared_counter, finish_counter, ready_counter;
int pi; // processor number in a main round
int stop = 0, terminate = 0;

FILE *log_file;

pthread_t *thds;
int *ids;
int *states;
void **rets;
pthread_mutex_t *mtxs;
pthread_cond_t *conds;
struct timespec *tss;

pthread_t main_thd;
pthread_mutex_t finish_mtx;
pthread_cond_t  finish_cond;
struct timespec ts_base;
struct timespec start_time, end_time;

#define aAdd1_32(cnt)  __sync_fetch_and_add((int32_t *)(&cnt), 1)

long long getTime (struct timespec* tp) {
  clock_gettime(CLOCK_MONOTONIC, tp);
  printf("start time: %d:%d\n", tp->tv_sec, tp->tv_nsec);
  return tp->tv_sec * (long long)1e9 + tp->tv_nsec;
}

long long deltaTime (struct timespec* tp1, struct timespec* tp2) {
  return (tp2->tv_sec - tp1->tv_sec) * (long long)1e9 + (tp2->tv_nsec - tp1->tv_nsec);
}

void *thread_func (void *args) {
  int thd_id = *((int *) args);

  pthread_mutex_lock(mtxs+thd_id);
  while (states[thd_id] == 0) {
    pthread_cond_wait(conds+thd_id, mtxs+thd_id);
  }
  states[thd_id] = 0;
  pthread_mutex_unlock(mtxs+thd_id);

  while (!stop) {
    int _round = w;
    // int _round = rand() % w;

    int j;
    for (int i = 0; i < _round; i++) {
      for (j = 0; j < N1; j++) {asm("");} /** void loop */
      int tmp_cnt = aAdd1_32(shared_counter);
      if (stop) break;
      // if (tmp_cnt % N2 == 0) {
      //   //log
      //   printf("reach %d!\n", tmp_cnt);
      // }
    }

    pthread_mutex_lock(&finish_mtx);
    finish_counter++;
    if (finish_counter == pi) {
      pthread_cond_signal (&finish_cond);
    }
    pthread_mutex_unlock(&finish_mtx);

    pthread_mutex_lock(mtxs + thd_id);
    while (states[thd_id] == 0) {
      pthread_cond_wait(conds+thd_id, mtxs+thd_id);
    }
    states[thd_id] = 0;
    pthread_mutex_unlock(mtxs + thd_id);
  }
  return NULL;
}

void Cycler_init(Int64 _p) {
  p = _p;

  thds  = (pthread_t *)       malloc (p * sizeof(pthread_t));
  ids   = (int *)             malloc (p * sizeof(int));
  states= (int *)             malloc (p * sizeof(int));
  rets  = (void **)           malloc (p * sizeof(void *));
  mtxs  = (pthread_mutex_t *) malloc (p * sizeof(pthread_mutex_t));
  conds = (pthread_cond_t *)  malloc (p * sizeof(pthread_cond_t));
  tss   = (struct timespec *) malloc (p * sizeof(struct timespec));

  shared_counter = finish_counter = ready_counter = 0;
  srand(time(NULL));
  for (int i = 0; i < p; i++) {
    pthread_cond_init (conds+i, NULL);
    pthread_mutex_init(mtxs +i, NULL);
    ids[i] = i; // set thread ids
    states[i] = 0;
  }
  pthread_cond_init (&finish_cond, NULL);
  pthread_mutex_init(&finish_mtx,  NULL);
  stop = 0;

#if SETAFFINITY == 1
  cpu_set_t cpuset;
  CPU_ZERO(&cpuset);
  for (int i = 0; i < p; i++) {
    pthread_create(&(thds[i]), NULL, thread_func, &(ids[i]));
    CPU_SET(i, &cpuset);
    pthread_setaffinity_np(thds[i], sizeof(cpu_set_t), &cpuset);
  }

  pthread_t self = pthread_self();
  CPU_SET(15, &cpuset);
  pthread_setaffinity_np(self, sizeof(cpu_set_t), &cpuset);
#else
  for (int i = 0; i < p; i++) {
    pthread_create(&(thds[i]), NULL, thread_func, &(ids[i]));
  }
#endif
  printf("Cycler initialization finished! thread num: %d\n", p);
}

void Cycler_cleanup () {
  for (int i = 0; i < p; i++) {
    pthread_mutex_lock(mtxs+i);
    states[i] = 1;
    pthread_mutex_unlock(mtxs+i);
    pthread_cond_signal(conds+i);
  }
}

void Cycler_terminate() {
  pthread_mutex_lock(&finish_mtx);
  stop = 1;
  pthread_mutex_unlock(&finish_mtx);
  pthread_cond_signal(&finish_cond);
  pthread_join(main_thd, NULL);
  printf("Cycler terminated!\n");
  Cycler_report();
}

void* Cycler_main(void *foo) {
  // printf("shared coutner : %d\n", shared_counter);
  clock_gettime(CLOCK_MONOTONIC, &start_time);

  while (stop == 0) {
    pi = p <= 1 ? 1 : (rand() % (p-1) + 1);
    // pi = p;
    for (int i = 0; i < pi; i++) {
      pthread_mutex_lock(mtxs+i);
      states[i] = 1;
      pthread_mutex_unlock(mtxs+i);
      pthread_cond_signal(conds+i);
    }

    pthread_mutex_lock(&finish_mtx);
    while (!stop && finish_counter < pi) {
      pthread_cond_wait(&finish_cond, &finish_mtx);
    }
    pthread_mutex_unlock(&finish_mtx);
    // printf("sc = %d\n", shared_counter);
  }
  clock_gettime(CLOCK_MONOTONIC, &end_time);
  return NULL;
}

void Cycler_begin() {
  if (p == 0) {
    return;
  } else {
    pthread_create(&main_thd, NULL, Cycler_main, NULL);
  }
}

void Cycler_report() {
  double dt = deltaTime(&start_time, &end_time);
  double Pa = 0.2688*shared_counter*N1/dt;

  printf("CYCLER: runtime: %.3lfs, counter: %d, Pa: %.3lf\n", dt/1e9, shared_counter, Pa);
}

// int main() {
//   Cycler_init ();

//   clock_gettime(CLOCK_MONOTONIC, &start_time);

//   while (1) {
//     // pi = p <= 1 ? 1 : (rand() % (p-1) + 1);
//     pi = p;
//     for (int i = 0; i < pi; i++) {
//       pthread_mutex_lock(mtxs+i);
//       states[i] = 1;
//       pthread_mutex_unlock(mtxs+i);
//       pthread_cond_signal(conds+i);
//     }

//     pthread_mutex_lock(&finish_mtx);
//     while (finish_counter < pi) {
//       pthread_cond_wait(&finish_cond, &finish_mtx);
//     }
//     pthread_mutex_unlock(&finish_mtx);

//     if (shared_counter >= W) {
//       // end_time = getTime(&ts_base);
//       clock_gettime(CLOCK_MONOTONIC, &end_time);
//       stop = 1;
//       break;
//     }
//   }
//   printf("shared_counter = %d, Total time = %lldns\n", shared_counter, deltaTime(&start_time, &end_time));

//   Cycler_cleanup();
//   return 0;
// }
