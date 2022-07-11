#include <mpi.h>
#include <stdint.h>

/*
  How to compile:

  gcc -Wall -std=c99 -lSDL2 -lpthread mandel.c -o mandel

  Optionally, you can specify the following too:
  -DMANDEL_ITER=<num>
    ~ controls the number of iterations to perform for each mandelbrot pixel.
  -DCPUS=<num_cpus>
    ~ controls the number of threads that are spun up for rendering.
 */

#include "common.h"

int world_size;
int world_rank;
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

  // I don't know if this does what I think it does
  MPI_Init(NULL, NULL);
  MPI_Comm_size(MPI_COMM_WORLD, &world_size);
  MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
  cpu_n_render_pixels(world_rank, world_size);
  MPI_Finalize();
}

int main() {
  /*
    Set our initial window width and height to be 800 x 600 - will be
    adjusted.

    Automatically scale the window width so that the number of CPUS will
    always cleanly divide.
   */

  /*
    TODO:
    * Look into long double buffering?
    https://stackoverflow.com/questions/28334892/sdl2-long
    double-buffer-not-working-still-tearing

    - Is it possible to have two 'images' that we can render to? So that way
    we can smoothly zoom in. So we can have the show_mandelbrot function
    always show the 'ready' buffer while 'render_mandelbrot' can always work
    on the next one?

    * Allow for zooming out

    * Work on coloring algorithm

    * Add optional gmp support / move to long long doubles and long ints.
   */

  set_window_dimension(world_size);
  if (init_sdl() == EXIT_FAILURE)
    return EXIT_FAILURE;
  sdl_mainloop();
  sdl_cleanup();
  return EXIT_SUCCESS;
}
