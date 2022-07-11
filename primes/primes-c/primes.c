#include <pthread.h>

#ifndef CPUS
#define CPUS 1
#endif

#include "common.h"

/*
  FIXME: There is a bug in this implementation (doesn't exist in the mpi
  version) where the threads will calculate the same primes.

  How to check:

  compare:
  timeout -s 2 5 tcc -run -DCPUS=2 primes.c | sort -u | wc -l

  to:
  timeout -s 2 5 tcc -run -DCPUS=2 primes.c | wc -l

  If they are radically different, you know there is a bug
 */

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
