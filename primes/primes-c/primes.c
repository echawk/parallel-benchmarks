#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>

#ifndef CPUS
#define CPUS 1
#endif

typedef unsigned long nt;

nt sqrt_nt(nt n) {
  nt x0 = n / 2; // Initial estimate
                 // Avoid overflow when s is the maximum representable value

  // Sanity check
  if (x0 != 0) {
    nt x1 = (x0 + n / x0) / 2;

    while (x1 < x0) {
      x0 = x1;
      x1 = (x0 + n / x0) / 2;
    }
    return x0 + 1;
  } else
    return n + 1;
}

bool isPrime(nt n) {
  int i = 0;
  if (n % 2 == 0)
    return false;
  for (i = 3; i <= sqrt_nt(n); i += 2) {
    if (n % i == 0)
      return false;
  }
  return true;
}

void check_prime_from(int start, int total_cpus) {
  nt init = (nt)start;
  nt step = (nt)CPUS;
  while (true) {
    if (isPrime(init))
      printf("%lu\n", init);
    init += step;
  }
}

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
