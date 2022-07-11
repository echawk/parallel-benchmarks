#include <pthread.h>

#ifndef CPUS
#define CPUS 1
#endif

#include "common.h"

void *thread_prime(void *arg) {
  int *v = (int *)arg;
  check_prime_from(v[0], CPUS);
  return NULL;
}

int main() {
  pthread_t pids[CPUS];
  int i, start = 0;

  for (i = 0; i < CPUS; i++) {
    start = 2 * i + 1;
    pthread_create(&pids[i], NULL, thread_prime, (void *)&start);
  }

  for (i = 0; i < CPUS; i++) {
    pthread_join(pids[i], NULL);
  }

  return 0;
}
