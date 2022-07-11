#include <mpi.h>
#include <stdint.h>

/*
  How to compile:

  mpicc -Wall -std=c99 -lSDL2 mpi_mandelbrot.c -o mpi_mandelbrot

  Optionally, you can specify the following too:
  -DMANDEL_ITER=<num>
    ~ controls the number of iterations to perform for each mandelbrot pixel.

  To execute:

  mpiexec ./mpi_mandelbrot

 */

#include "common.h"

void render_mandelbrot() {
  /*
    The picture is effecively divided up into 'columns' -

                  WIDTH

    !@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!
    !@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!
    !@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!
    !@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!
    !@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!  HEIGHT
    !@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!
    !@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!
    !@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!
    !@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!
    !@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!@!


    Each thread gets its own 'column'.
   */

  // Broadcast render message to the other threads.
  // cpu_n_render_pixels(world_rank, world_size);
  int world_rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
  int world_size;
  MPI_Comm_size(MPI_COMM_WORLD, &world_size);
  int render_flag;
  if (world_rank == 0) {
    printf("CPU %d is broadcasting!\n", world_rank);
    MPI_Bcast(&render_flag, 1, MPI_INT, 0, MPI_COMM_WORLD);
  }
}

int main() {

  MPI_Init(NULL, NULL);

  int world_rank;
  MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
  int world_size;
  MPI_Comm_size(MPI_COMM_WORLD, &world_size);

  int render_flag;

  if (world_rank == 0) {

    set_window_dimension(world_size - 1);
    if (init_sdl() == EXIT_FAILURE)
      return EXIT_FAILURE;
    sdl_mainloop();
    sdl_cleanup();
    return EXIT_SUCCESS;

  } else {
    MPI_Bcast(&render_flag, 1, MPI_INT, 0, MPI_COMM_WORLD);
    if (render_flag == 1) {
      printf("CPU %d is rendering!\n", world_rank);
      cpu_n_render_pixels(world_rank - 1, world_size - 1);
    }
  }
  MPI_Finalize();
}
