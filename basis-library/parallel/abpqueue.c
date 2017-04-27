#define tag(A) = A >> 16
#define top(A) = (A << 48) >> 48
#define packAge(G,P) = (G << 16) + P

#define MAXSIZE 1 << 16

typedef struct {
  unsigned int age;
  unsigned int bot;
  void **deq;
} deque

int ABP_pushBottom (deque *d, void *node) {
  unsigned int bot = d->bot;
  if (bot > MAXSIZE) return -1;
  d->deq[bot] = node;
  d->bot = bot + 1;
}

void *ABP_popTop (deque *d) {
  unsigned int oldAge = d->age;
  unsigned int bot = d->bot;

  if (bot <= top(oldAge))
    return NULL;

  void *node = d->deq[top(oldAge)];

  unsigned int newAge = packAge(tag(oldAge), top(oldAge) + 1);

  if (__sync_bool_compare_and_swap (&d->age, oldAge, newAge))
    return node;
  else
    return NULL;
}

void *ABP_popBottom (deque *d) {
  unsigned int bot = d->bot;
  if (bot = 0) return NULL;

  bot--;
  d->bot = bot;

  void *node = d->deq[bot];
  unsigned int oldAge = d->age;

  if (bot > top(oldAge))
    return node;

  d->bot = 0;
  unsigned int newAge = packAge(tag(oldAge) + 1, 0);
  if ((bot == top(oldAge)) &&
      (__sync_bool_compare_and_seawp(&d->age, oldAge, newAge)))
    return node;
  d->age = newAge;
  return NULL;
}

deque *ABP_newDeque() {
  deque d = malloc(2 * sizeof(unsigned int) + sizeof(void**));
  d.age = packAge(0, 0);
  d.bot = 0;
  d.deq = malloc(MAXSIZE * sizeof(void*));
  return &d;
}

void *getRef (void *r) {
  return r;
}
