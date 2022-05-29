#include <complex.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <unistd.h>

#include <SDL2/SDL.h>

#ifndef CPUS
#define CPUS 1
#endif

/*
  How to compile:

  $ gcc -Wall -std=c99 -DCPUS=<num_cpus> -lSDL2 -lpthread mandel.c -o mandel

 */

struct rgb {
  int r, g, b;
};
typedef struct rgb rgb_T;

struct pixel {
  int x, y;
  rgb_T color;
};
typedef struct pixel pixel_T;

void print_pixel_T(pixel_T *px) {
  printf("(%d, %d) - R%d G%d B%d\n", px->x, px->y, px->color.r, px->color.g,
         px->color.b);
}

rgb_T rand_rgb() {
  int r = rand() % 255;
  int g = rand() % 255;
  int b = rand() % 255;
  return (rgb_T){.r = r, .g = g, .b = b};
}

int W_WIDTH;
int W_HEIGHT;
void *window;
void *renderer;
pthread_mutex_t mutex;

rgb_T color = {.r = 100, .g = 0, .b = 0};

/*
  FIXME: come up with an easy way to cacluate the mandelbrot pixels as well,
  since the current pixels are for the window. Each pixel on the window is going
  to have a relationship with the 'pixels' of the mandelbrot image.

  Bascially the main window will be a magnifing window of the mandelbrot set and
  it will serve to just show the information. However there the color at a
  particular pixel will instead be based off of the actual coordinates of
  wherever we are in the mandelbrot image.
 */

float MANDEL_Y_MIN = -1.5;
float MANDEL_Y_MAX = 1.5;
float MANDEL_X_MIN = -2.0;
float MANDEL_X_MAX = 2.0;

bool check_mandel_proportions() {
  float diff_x_max_min = MANDEL_X_MAX - MANDEL_X_MIN;
  float diff_y_max_min = MANDEL_Y_MAX - MANDEL_Y_MIN;
  float diff_to_win_prop = (.75 - (diff_y_max_min / diff_x_max_min));
  diff_to_win_prop =
      diff_to_win_prop < 0 ? -diff_to_win_prop : diff_to_win_prop;
  return diff_to_win_prop < 0.001;
}

#ifndef MANDEL_ITER
#define MANDEL_ITER 80
#endif

rgb_T iter_to_rgb(int iter) {
  int lowest_third = MANDEL_ITER / 3;
  int middle_third = 2 * lowest_third;
  float percentile;
  if (iter < lowest_third) {
    /* Blue Dominated*/
    percentile = (float)iter / lowest_third;
    int max_mag = (int)255 * percentile + 50;
    return (rgb_T){.r = .5 * max_mag, .g = .33 * max_mag, .b = max_mag};
  } else if (iter < middle_third) {
    /* Red Dominated*/
    percentile = (float)iter / middle_third;
    int max_mag = (int)255 * percentile + 50;
    return (rgb_T){.r = max_mag, .g = .33 * max_mag, .b = .5 * max_mag};
  } else {
    /* Green Dominated*/
    percentile = (float)iter / MANDEL_ITER;
    int max_mag = (int)255 * percentile + 50;
    return (rgb_T){.r = max_mag, .g = .5 * max_mag, .b = .33 * max_mag};
  }
}

rgb_T window_x_y_to_color(int x_pixel, int y_pixel) {
  float y = ((float)y_pixel / W_HEIGHT) * (MANDEL_Y_MAX - MANDEL_Y_MIN) +
            MANDEL_Y_MIN;
  float x = ((float)x_pixel / (W_WIDTH - 1)) * (MANDEL_X_MAX - MANDEL_X_MIN) +
            MANDEL_X_MIN;
  float x0 = x;
  float y0 = y;
  for (int i = 0; i < MANDEL_ITER; i++) {
    float x1 = (x0 * x0) - (y0 * y0);
    float y1 = 2 * x0 * y0;

    x1 = x1 + (float)x;
    y1 = y1 + (float)y;

    x0 = x1;
    y0 = y1;

    float d = (x0 * x0) + (y0 * y0);
    if (d > 4) {
      return iter_to_rgb(i);
    }
  }
  return (rgb_T){.r = 0, .g = 0, .b = 0};
}

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
      //((pixel_T){.x = i, .y = j, .color = red});
      px_col = window_x_y_to_color(i, j);
      pthread_mutex_lock(&mutex);
      SDL_SetRenderDrawColor(renderer, px_col.r, px_col.g, px_col.b, 255);
      SDL_RenderDrawPoint(renderer, i, j);
      pthread_mutex_unlock(&mutex);
    }
  }
}

void *thread_render(void *arg) {
  int *v = (int *)arg;
  cpu_n_render_pixels(*v);
  return NULL;
}

int main() {
  srand(0);
  /*
    Set our initial window width and height to be 800 x 600 - will be
    adjusted.

    Automatically scale the window width so that the number of CPUS will
    always cleanly divide.
   */

  W_WIDTH = 800 - (800 % CPUS);
  W_HEIGHT = 600;
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

  SDL_RenderClear(renderer);
  SDL_RenderPresent(renderer);

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

  pthread_t pids[CPUS];
  for (int z = 0; z < 10; z++) {
    for (int i = 0; i < CPUS; i++) {
      pthread_create(&pids[i], NULL, thread_render, (void *)&i);
    }
    for (int i = 0; i < CPUS; i++) {
      pthread_join(pids[i], NULL);
    }
    SDL_RenderPresent(renderer);
    sleep(1);
    color = rand_rgb();
  }

  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);
  SDL_Quit();

  return 0;
}
