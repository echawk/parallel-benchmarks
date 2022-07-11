#include <pthread.h>
#include <stdint.h>

#ifndef CPUS
#define CPUS 4
#endif

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

/*
  FIXME: come up with an easy way to cacluate the mandelbrot pixels as well,
  since the current pixels are for the window. Each pixel on the window is going
  to have a relationship with the 'pixels' of the mandelbrot image.

  Bascially the main window will be a magnifing window of the mandelbrot set and
  it will serve to just show the information. However there the color at a
  particular pixel will instead be based off of the actual coordinates of
  wherever we are in the mandelbrot image.
 */

/*
  Current plan for multi threading is to have 'workers' that each are
  responsible for rendering specific pixels/areas. When done with their
  work, they'll signal to the main thread that they are done. The main
  thread waits to receive all of the 'dones' from the worker threads, and
  once all work is done, the main thread presents the now rendered image.

  pthreads will be responsible for perfomring the operations associated with
  each. Each thread will know what 'CPU' it is, and thus know exactly which
  pixels it will need to render soley from that information.
 */
void *thread_render(void *arg) {
  int *v = (int *)arg;
  cpu_n_render_pixels(*v, CPUS);
  return NULL;
}

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
  pthread_t pids[CPUS];
  for (int i = 0; i < CPUS; i++) {
    pthread_create(&pids[i], NULL, thread_render, (void *)&i);
  }
  for (int i = 0; i < CPUS; i++) {
    pthread_join(pids[i], NULL);
  }
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

  set_window_dimension(CPUS);
  if (init_sdl() == EXIT_FAILURE)
    return EXIT_FAILURE;
  sdl_mainloop();
  sdl_cleanup();
  return EXIT_SUCCESS;
}
