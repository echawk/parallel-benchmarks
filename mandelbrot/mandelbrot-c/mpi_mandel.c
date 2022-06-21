#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h>
//  SDL includes
#include <SDL2/SDL.h>
#include <SDL2/SDL_error.h>
#include <SDL2/SDL_events.h>
#include <SDL2/SDL_render.h>
#include <SDL2/SDL_keycode.h>
//	defines
#ifndef MAX_ITER
#define MAX_ITER 80
#endif
#ifndef WIN_WIDTH
#define WIN_WIDTH 800
#endif
#ifndef WIN_HEIGHT
#define WIN_HEIGHT 600
#endif
//  typedefs
typedef long double ldouble;
typedef struct {
  uint8_t r, g, b;
} rgb_t;
//  global vars (bad and terrible, but necessary to keep my sanity)
ldouble gl_x_max =  2.0;
ldouble gl_x_min = -2.0;
ldouble gl_y_max =  1.5;
ldouble gl_y_min = -1.5;

//  0 to n from a to b
//  {0...n} / n = {0....1}
//  {0...1} * (b-a) = {0...b-a}
//  {0...b-a} + a = {a...b-a+a} = {a...b}
//  scales a number to be within a specific range of values
ldouble scale(ldouble num, ldouble old_max, ldouble min, ldouble max){
  return ((num / old_max) * (max - min)) + min;
}
//  maps the iteration to the color
rgb_t get_color_from_iteration(int iter){

  ldouble h, s, v, c, x, m;
  rgb_t rgb_prime;

  if(iter == MAX_ITER){
    return (rgb_t){0, 0, 0};
  }
  else{
    //h = scale(iter, MAX_ITER, 0, 240);
    //h = (((ldouble)iter/(ldouble)MAX_ITER) * scale) + min;
    h = iter % 360;
    s = 1;
    v = 1;
    c = v * s;
    x = c * (1 - fabsl((remainderl(h / 60.0, 2) - 1)));
    m = v - c;
    if(h < 60){
      rgb_prime = (rgb_t){c, x, 0};
    }
    else if(h < 120){
      rgb_prime = (rgb_t){x, c, 0};
    }
    else if(h < 180){
      rgb_prime = (rgb_t){0, c, x};
    }
    else if(h < 240){
      rgb_prime = (rgb_t){0, x, c};
    }
    else if(h < 300){
      rgb_prime = (rgb_t){x, 0, c};
    }
    else if(h < 360){
      rgb_prime = (rgb_t){c, 0, x};
    }

    rgb_prime.r = (rgb_prime.r + m)*255;
    rgb_prime.g = (rgb_prime.g + m)*255;
    rgb_prime.b = (rgb_prime.b + m)*255;

    return rgb_prime;
  }
}
//  the main mandelbrot algorithm
rgb_t get_color_at_pt(int xpt, int ypt){
  ldouble y0 = ((ldouble)ypt / WIN_HEIGHT) * (gl_y_max - gl_y_min) + gl_y_min;
  ldouble x0 = ((ldouble)xpt / (WIN_WIDTH - 1)) * (gl_x_max - gl_x_min) + gl_x_min;
  ldouble x = 0.0;
  ldouble y = 0.0;
  int iteration = 0;
  while(iteration < MAX_ITER && x*x + y*y <= 4){
    ldouble xtemp = x*x - y*y + x0;
    y = 2*x*y + y0;
    x = xtemp;

    iteration++;
  }
  return get_color_from_iteration(iteration);
}
//  renders the current section of mandelbrot
void render_mandelbrot(SDL_Renderer* renderer){
  ldouble xscale = fabsl(gl_x_min - gl_x_max)/(ldouble)WIN_WIDTH;
  ldouble yscale = fabsl(gl_y_min - gl_y_max)/(ldouble)WIN_HEIGHT;

  for(ldouble x0 = gl_x_min; x0 < gl_x_max; x0 += xscale){
		for(ldouble y0 = gl_y_min; y0 < gl_y_max; y0 += yscale){
			rgb_t col = get_color_at_pt(x0, y0);

			SDL_SetRenderDrawColor(renderer, col.r, col.g, col.b, 255);
			SDL_RenderDrawPoint(renderer, x0, y0);
		}
	}
}
//  handles the keyboard input from the user
// void handle_kb_input(SDL_Event e){
//   switch(e.key.keysym.sym){
//     case SDLK_w:
//     case SDLK_UP:
//       pan();
//       break;
//     case SDLK_a:
//     case SDLK_LEFT:
//       pan();
//       break;
//     case SDLK_s:
//     case SDLK_DOWN:
//       pan();
//       break;
//     case SDLK_d:
//     case SDLK_RIGHT:
//       pan();
//       break;
//     case SDLK_PLUS:
//       zoom_in();
//       break;
//     case SDLK_MINUS:
//       zoom_out();
//       break;
//   }
// }

int main(int argc, char** argv){
  // int chunk_width, num_processes, rank;
  //
  // MPI_Init(&argc, &argv);
	// MPI_Comm_size(MPI_COMM_WORLD, &num_processes);
	// MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  //
  // chunk_width = (WIN_WIDTH / num_processes) + ((WIN_WIDTH % num_processes) == 0 ? 0 : 1);
  if(!SDL_Init(SDL_INIT_VIDEO)){
    SDL_Window *window = NULL;
    SDL_Renderer *renderer = NULL;
    if(!SDL_CreateWindowAndRenderer(640, 480, 0, &window, &renderer)){
      bool running = true;

      while(running){
        SDL_Event event;
        while(SDL_PollEvent(&event)){
          switch(event.type){
            case SDL_QUIT:
              running = 0;
              break;
            case SDL_KEYDOWN:
              //handle_kb_input(event);
              break;
          }
          render_mandelbrot(renderer);
          SDL_RenderPresent(renderer);
        }
      }
    }
    if(renderer)
      SDL_DestroyRenderer(renderer);
    if(window)
      SDL_DestroyWindow(window);
  }
  return EXIT_SUCCESS;
}
