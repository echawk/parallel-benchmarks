#include <mpi.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdint.h>
#include <stdbool.h>
//	SDL2 includes
#include <SDL2/SDL.h>
#include <SDL2/SDL_error.h>
#include <SDL2/SDL_render.h>
#include <SDL2/SDL_events.h>
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
//	typedefs
typedef long double ld;
typedef struct {
	uint8_t r, g, b;
} rgb_T;
//	enums
enum tag {SHUTDOWN_TAG, RENDER_TAG, RENDERING_DONE_TAG};
enum direction {UP, DOWN, LEFT, RIGHT};
//	constants
const int POINTER_SIZE = sizeof(void*);
//	functions
//	gets the minimum value between two integers
int imin(int a, int b){
	return ((a < b) ? a : b);
}
//	initializes SDL, window, and renderer and returns success if it worked or failure if it didn't
int init_sdl(SDL_Window* window, SDL_Renderer* renderer){
	if(SDL_Init(SDL_INIT_VIDEO) != 0){
		fprintf(stderr, "[ERR] Could not initialize SDL2: %s\n", SDL_GetError());
		return EXIT_FAILURE;
	}
	window = SDL_CreateWindow("Mandelbrot - C", 0, 0, WIN_WIDTH, WIN_HEIGHT, SDL_WINDOW_SHOWN);
	if(window == NULL){
		fprintf(stderr, "[ERR] SDL_CreateWindow failed: %s\n", SDL_GetError());
		return EXIT_FAILURE;
	}
	renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
	if(renderer == NULL){
		SDL_DestroyWindow(window);
		fprintf(stderr, "[ERR] SDL_CreateRenderer failed: %s", SDL_GetError());
		return EXIT_FAILURE;
	}
	return EXIT_SUCCESS;
}
//	pans the view
void pan(int dir, ld* ymax, ld* ymin, ld* xmax, ld* xmin){
  int pixel_size = 10;
  ld y_delta = ((ld)pixel_size / WIN_HEIGHT) * (*ymax - *ymin);
  ld x_delta = ((ld)pixel_size /  WIN_WIDTH) * (*xmax - *xmin);
  switch(dir){
  case UP:
    *ymax -= y_delta;
    *ymin -= y_delta;
    break;
  case DOWN:
    *ymax += y_delta;
    *ymin += y_delta;
    break;
  case LEFT:
    *xmax -= x_delta;
    *xmin -= x_delta;
    break;
  case RIGHT:
    *xmax += x_delta;
    *xmin += x_delta;
    break;
  }
}
//	handles keyboard input
void handle_key(SDL_Event e, ld* ymax, ld* ymin, ld* xmax, ld* xmin){
  switch (e.key.keysym.sym) {
  case SDLK_LEFT:
    pan(LEFT, ymax, ymin, xmax, xmin);
    break;
  case SDLK_RIGHT:
    pan(RIGHT, ymax, ymin, xmax, xmin);
    break;
  case SDLK_UP:
    pan(UP, ymax, ymin, xmax, xmin);
    break;
  case SDLK_DOWN:
    pan(DOWN, ymax, ymin, xmax, xmin);
    break;
  }
}
//	zooms the view in at the specified point
void zoom_in(ld x_pt, ld y_pt, ld* ymax, ld* ymin, ld* xmax, ld* xmin){
  ld new_x_range = (*xmax - *xmin) * .8;
  ld new_y_range = (*ymax - *ymin) * .8;
  ld cy = y_pt * (*ymax - *ymin) + *ymin;
  ld cx = x_pt * (*xmax - *xmin) + *xmin;

  *xmax = cx + (.5 * new_x_range);
  *xmin = cx - (.5 * new_x_range);

  *ymax = cy + (.5 * new_y_range);
  *ymin = cy - (.5 * new_y_range);
}
//	sends a message to all nodes except the one sending the message
void MPI_Bcast_except(void* buffer, int count, MPI_Datatype datatype, int sender_rank, int tag, MPI_Comm communicator, int num_procs){
	for(int i = 0; i < num_procs - 1; i++){
		MPI_Send(buffer, count, datatype, (sender_rank + i + 1) % num_procs, tag, communicator);
	}
}
//	gives a color based on the number of iterations the point reached
rgb_T iter_to_rgb(int iter) {
  int lowest_third = MAX_ITER / 3;
  int middle_third = 2 * lowest_third;
  ld percentile;
  if (iter < lowest_third) {
    /* Blue Dominated*/
    percentile = (ld)iter / lowest_third;
    int max_mag = (int)255 * percentile + 50;
    return (rgb_T){.r = .5 * max_mag, .g = .33 * max_mag, .b = max_mag};
  } else if (iter < middle_third) {
    /* Red Dominated*/
    percentile = (ld)iter / middle_third;
    int max_mag = (int)255 * percentile + 50;
    return (rgb_T){.r = max_mag, .g = .33 * max_mag, .b = .5 * max_mag};
  } else {
    /* Green Dominated*/
    percentile = (ld)iter / MAX_ITER;
    int max_mag = (int)255 * percentile + 50;
    return (rgb_T){.r = .5 * max_mag, .g = max_mag, .b = .33 * max_mag};
  }
}
//	gets the color of the given point on the mandelbrot fractal
rgb_T get_color_from_pt(int x_pt, int y_pt, ld* ymax, ld* ymin, ld* xmax, ld* xmin){
	ld y = ((ld)y_pt / WIN_HEIGHT) * (*ymax - *ymin) + *ymin;
  ld x = ((ld)x_pt / (WIN_WIDTH - 1)) * (*xmax - *xmin) + *xmin;
  ld x0 = x;
  ld y0 = y;
  for (int i = 0; i < MAX_ITER; i++) {
    ld x1 = (x0 * x0) - (y0 * y0);
    ld y1 = 2 * x0 * y0;

    x1 = x1 + (ld)x;
    y1 = y1 + (ld)y;

    x0 = x1;
    y0 = y1;

    ld d = (x0 * x0) + (y0 * y0);
    if (d > 4) {
      return iter_to_rgb(i);
    }
  }
  return (rgb_T){.r = 0, .g = 0, .b = 0};
}
//	generates all of the colors in the specified chunk of the window
void render_pixels(SDL_Renderer* renderer, int rank, int chunk_width, ld* ymax, ld* ymin, ld* xmax, ld* xmin){
	for(int x0 = chunk_width * (rank - 1); x0 < imin(chunk_width * rank, WIN_WIDTH); x0++){
		for(int y0 = 0; y0 < WIN_HEIGHT; y0++){
			rgb_T col = get_color_from_pt(x0, y0, ymax, ymin, xmax, xmin);

			SDL_SetRenderDrawColor(renderer, col.r, col.g, col.b, 255);
			SDL_RenderDrawPoint(renderer, x0, y0);
		}
	}
}

int main(int argc, char**argv){
	ld mandel_y_max =  1.5;
	ld mandel_y_min = -1.5;
	ld mandel_x_max =  2.0;
	ld mandel_x_min = -2.0;

	int chunk_width;
	int num_processes, rank;

	bool running = true;

	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &num_processes);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);

	chunk_width = (WIN_WIDTH / num_processes) + ((WIN_WIDTH % num_processes) == 0 ? 0 : 1);

	if(num_processes < 2){
		printf("[ERR] This application needs at least 2 processes.\n");
		MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
	}

	if(rank == 0){
		SDL_Window* window = NULL;
		SDL_Renderer* renderer = NULL;

		if(init_sdl(window, renderer) == EXIT_FAILURE){
			printf("[ERR] SDL initialization failed.\n");
			MPI_Abort(MPI_COMM_WORLD, EXIT_FAILURE);
		}

		SDL_RenderClear(renderer);
		SDL_RenderPresent(renderer);
		while(running){
			SDL_Event e;

			while(SDL_PollEvent(&e)){
        switch(e.type){
	        case SDL_QUIT:
	          running = false;
						for(int i = 1; i < num_processes; i++)	//send to all other processes
							MPI_Send(&running, 1, MPI_C_BOOL, i, SHUTDOWN_TAG, MPI_COMM_WORLD);
            // MPI_Bcast_except(&running, 1, MPI_C_BOOL, rank, SHUTDOWN_TAG, MPI_COMM_WORLD, num_processes)
	          break;
	        case SDL_MOUSEBUTTONDOWN:
	          zoom_in((ld)e.button.x / WIN_WIDTH,
	                  (ld)e.button.y / WIN_HEIGHT,
										&mandel_y_max,
										&mandel_y_min,
										&mandel_x_max,
										&mandel_x_min);
	          break;
	        case SDL_KEYDOWN:
						handle_key(e, &mandel_y_max, &mandel_y_min, &mandel_x_max, &mandel_x_min);
	          break;
				}
    	}
      for(int i = 1; i < num_processes; i++)	//send to all other processes
        MPI_Send(renderer, POINTER_SIZE, MPI_BYTE, i, RENDER_TAG, MPI_COMM_WORLD);
			// MPI_Bcast_except(renderer, POINTER_SIZE, MPI_BYTE, rank, RENDER_TAG, MPI_COMM_WORLD, num_processes);
			MPI_Barrier(MPI_COMM_WORLD);	//wait for the rendering to be done
			SDL_RenderPresent(renderer);
		}

		SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
	}
	else{
		int exit_flag;
		SDL_Renderer* passed_renderer = NULL;
		MPI_Status status;
		MPI_Request exit_req;
		while(running){
			MPI_Irecv(&running, 1, MPI_C_BOOL, 0, SHUTDOWN_TAG, MPI_COMM_WORLD, &exit_req);
			MPI_Recv(passed_renderer, POINTER_SIZE, MPI_BYTE, 0, RENDER_TAG, MPI_COMM_WORLD, &status);
			render_pixels(passed_renderer, rank, chunk_width, &mandel_y_max
																											, &mandel_y_min
																											, &mandel_x_max
																											, &mandel_x_min);
			MPI_Barrier(MPI_COMM_WORLD);
			MPI_Test(&exit_req, &exit_flag, MPI_STATUS_IGNORE);
			if(exit_flag){
				running = false;
			}
		}
	}

	MPI_Finalize();
	return EXIT_SUCCESS;
}
