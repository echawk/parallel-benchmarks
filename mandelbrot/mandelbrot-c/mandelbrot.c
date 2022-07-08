#include <pthread.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <SDL2/SDL.h>
#include <SDL2/SDL_error.h>
#include <SDL2/SDL_events.h>
#include <SDL2/SDL_keycode.h>
#include <SDL2/SDL_render.h>

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

void *window;
void *renderer;

pthread_mutex_t renderer_mutex;

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
void cpu_n_render_pixels(int cpu_n) {
  /*
    FIXME: With 3 CPUS Black bars appear - I don't know why but it doesn't
   happen when the number of CPUs is 1 or 2.
  */

  rgb_T px_col;
  for (int i = cpu_n; i <= W_WIDTH; i += CPUS) {
    for (int j = 0; j <= W_HEIGHT; j++) {
      px_col = window_x_y_to_color(i, j);
      /*
        One thing to investigate would be if sdl2's mutexes provide a faster
        turn around time compared to pthreads.
        To create mutexes in sdl2 you use this - SDL_CreateMutex();
       */
      pthread_mutex_lock(&renderer_mutex);
      SDL_SetRenderDrawColor(renderer, px_col.r, px_col.g, px_col.b, 255);
      SDL_RenderDrawPoint(renderer, i, j);
      pthread_mutex_unlock(&renderer_mutex);
    }
  }
}

void *thread_render(void *arg) {
  int *v = (int *)arg;
  cpu_n_render_pixels(*v);
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

void show_mandelbrot() { SDL_RenderPresent(renderer); }

int init_sdl() {
  if (SDL_Init(SDL_INIT_VIDEO) != 0) {
    fprintf(stderr, "[ERR] Could not initialize sdl2: %s\n", SDL_GetError());
    return EXIT_FAILURE;
  }
  window = SDL_CreateWindow("MandelBrot - C", 0, 0, W_WIDTH, W_HEIGHT,
                            SDL_WINDOW_SHOWN);
  if (window == NULL) {
    fprintf(stderr, "[ERR] SDL_CreateWindow failed: %s\n", SDL_GetError());
    return EXIT_FAILURE;
  }
  renderer = SDL_CreateRenderer(
      window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
  if (renderer == NULL) {
    SDL_DestroyWindow(window);
    fprintf(stderr, "[ERR] SDL_CreateRenderer failed: %s", SDL_GetError());
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

void handle_key(SDL_Event e) {
  switch (e.key.keysym.sym) {
  case SDLK_LEFT:
    pan(LEFT);
    break;
  case SDLK_RIGHT:
    pan(RIGHT);
    break;
  case SDLK_UP:
    pan(UP);
    break;
  case SDLK_DOWN:
    pan(DOWN);
    break;
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

  W_WIDTH = 800 - (800 % CPUS);
  W_HEIGHT = 600;

  if (init_sdl() == EXIT_FAILURE)
    return EXIT_FAILURE;

  SDL_RenderClear(renderer);
  SDL_RenderPresent(renderer);

  SDL_Event e;
  bool should_exit = false;
  while (!should_exit) {
    while (SDL_PollEvent(&e)) {
      switch (e.type) {
      case SDL_QUIT:
        should_exit = true;
        break;
      case SDL_MOUSEBUTTONDOWN:
        zoom_on_point((long double)e.button.x / W_WIDTH,
                      (long double)e.button.y / W_HEIGHT);
        break;
      case SDL_KEYDOWN:
        handle_key(e);
        break;
      }
    }
    render_mandelbrot();
    show_mandelbrot();
  }

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();

  return EXIT_SUCCESS;
}
