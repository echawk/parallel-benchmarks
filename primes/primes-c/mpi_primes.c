#include <mpi.h>

#include "common.h"

int main() {
  int world_size;
  MPI_Init(NULL, NULL);
  MPI_Comm_size(MPI_COMM_WORLD, &world_size);
  int world_rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
  check_prime_from(2 * world_rank + 1, world_size);
  MPI_Finalize();
  return 1;
}
