#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#ifdef USE_MPI
#include <mpi.h>
#else
#include <pthread.h>
#endif

#ifndef CPUS
#define CPUS 1
#endif
/*
   To compile:
   cc -DCPUS=<num_cpu> -lpthread factorize.c -o factorize

   Or run directly with tcc:
   tcc -DCPUS=<num_cpu> -lpthread -run factorize.c

   To compile (with MPI):
   mpicc -DUSE_MPI factorize.c -o factorize

   To run (with MPI):
   mpiexec ./factorize

*/

void factorize(int n) {
  int i;
  printf("(");
  for (i = 1; i <= n; i++) {
    if (n % i == 0) {
      printf(" %d", i);
    }
  }
  printf(")\n");
}

void factorize_start_from(int start, int total_cpus) {
  while (1) {
    factorize(start);
    start += total_cpus;
  }
}

void *thread_fact(void *arg) {
  int *v = (int *)arg;
  factorize_start_from(v[0], CPUS);
  return NULL;
}

int main() {
#ifndef USE_MPI
  pthread_t pids[CPUS];

  int i, start = 0;

  for (i = 0; i < CPUS; i++) {
    start = i + 1;
    pthread_create(&pids[i], NULL, thread_fact, (void *)&start);
  }

  for (i = 0; i < CPUS; i++) {
    pthread_join(pids[i], NULL);
  }
#else
  int world_size;
  MPI_Init(NULL, NULL);
  MPI_Comm_size(MPI_COMM_WORLD, &world_size);
  int world_rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
  factorize_start_from(world_rank, world_size);
  MPI_Finalize();
#endif
  return 1;
}
