#include <mpi.h>
#include <stdbool.h>
#include <stdio.h>

int main() {
  MPI_Init(NULL, NULL);
  int world_rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);

  int data;
  while (true) {
    if (world_rank == 0) {
      data = 100;
      printf("CPU %d broadcasting data %d\n", world_rank, data);
      MPI_Bcast(&data, 1, MPI_INT, 0, MPI_COMM_WORLD);
    } else {
      MPI_Bcast(&data, 1, MPI_INT, 0, MPI_COMM_WORLD);
      printf("CPU %d data %d\n", world_rank, data);
    }
    MPI_Barrier(MPI_COMM_WORLD);
  }

  MPI_Finalize();
  return 0;
}
