#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define CPUS 4

/*
   To compile:
   cc -lpthread factorize.c
*/

void factorize(int n) {
  printf("(");
  for (int i = 1; i <= n; i++) {
    if (n % i == 0) {
      printf(" %d", i);
    }
  }
  printf(")\n");
}

void factorize_start_from(int start) {
  while (1) {
    factorize(start);
    start += CPUS;
  }
}

void *thread_fact(void *arg) {
  int *v = (int *)arg;
  factorize_start_from(v[0]);
  return NULL;
}

int main() {

  pthread_t pids[CPUS];

  int i, start = 0;

  for (i = 0; i < CPUS; i++) {
    start = i + 1;
    pthread_create(&pids[i], NULL, thread_fact, (void *)&start);
  }

  for (i = 0; i < CPUS; i++) {
    pthread_join(pids[i], NULL);
  }

  return 1;
}
