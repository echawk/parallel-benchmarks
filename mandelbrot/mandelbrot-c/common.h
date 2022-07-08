#include <stdint.h>

typedef struct {
  uint8_t r, g, b;
} rgb_T;

int W_WIDTH;
int W_HEIGHT;

long double MANDEL_Y_MIN = -1.5;
long double MANDEL_Y_MAX = 1.5;
long double MANDEL_X_MIN = -2.0;
long double MANDEL_X_MAX = 2.0;

bool check_mandel_proportions() {
  long double diff_x_max_min = MANDEL_X_MAX - MANDEL_X_MIN;
  long double diff_y_max_min = MANDEL_Y_MAX - MANDEL_Y_MIN;
  long double diff_to_win_prop = (.75 - (diff_y_max_min / diff_x_max_min));
  diff_to_win_prop =
      diff_to_win_prop < 0 ? -diff_to_win_prop : diff_to_win_prop;
  return diff_to_win_prop < 0.001;
}

#ifndef MANDEL_ITER
#define MANDEL_ITER 80
#endif

rgb_T iter_to_rgb(int iter) {

  switch (iter % 16) {
  case 0:
    return (rgb_T){.r = 66, .g = 30, .b = 15};
  case 1:
    return (rgb_T){.r = 25, .g = 7, .b = 26};
  case 2:
    return (rgb_T){.r = 9, .g = 1, .b = 47};
  case 3:
    return (rgb_T){.r = 4, .g = 4, .b = 73};
  case 4:
    return (rgb_T){.r = 0, .g = 7, .b = 100};
  case 5:
    return (rgb_T){.r = 12, .g = 44, .b = 138};
  case 6:
    return (rgb_T){.r = 24, .g = 82, .b = 177};
  case 7:
    return (rgb_T){.r = 57, .g = 125, .b = 209};
  case 8:
    return (rgb_T){.r = 134, .g = 181, .b = 229};
  case 9:
    return (rgb_T){.r = 211, .g = 236, .b = 248};
  case 10:
    return (rgb_T){.r = 241, .g = 233, .b = 191};
  case 11:
    return (rgb_T){.r = 248, .g = 201, .b = 95};
  case 12:
    return (rgb_T){.r = 255, .g = 170, .b = 0};
  case 13:
    return (rgb_T){.r = 204, .g = 128, .b = 0};
  case 14:
    return (rgb_T){.r = 153, .g = 87, .b = 0};
  case 15:
    return (rgb_T){.r = 106, .g = 52, .b = 3};
  }
}

rgb_T window_x_y_to_color(int x_pixel, int y_pixel) {
  /*
    This is the actual mandelbrot algorithm.
   */
  long double y =
      ((long double)y_pixel / W_HEIGHT) * (MANDEL_Y_MAX - MANDEL_Y_MIN) +
      MANDEL_Y_MIN;
  long double x =
      ((long double)x_pixel / (W_WIDTH - 1)) * (MANDEL_X_MAX - MANDEL_X_MIN) +
      MANDEL_X_MIN;
  long double x0 = x;
  long double y0 = y;
  for (int i = 0; i < MANDEL_ITER; i++) {
    long double x1 = (x0 * x0) - (y0 * y0);
    long double y1 = 2 * x0 * y0;

    x1 = x1 + (long double)x;
    y1 = y1 + (long double)y;

    x0 = x1;
    y0 = y1;

    long double d = (x0 * x0) + (y0 * y0);
    if (d > 4) {
      return iter_to_rgb(i);
    }
  }
  return (rgb_T){.r = 0, .g = 0, .b = 0};
}

void zoom_on_point(long double xp, long double yp) {
  long double new_x_range = (MANDEL_X_MAX - MANDEL_X_MIN) * .8;
  long double new_y_range = (MANDEL_Y_MAX - MANDEL_Y_MIN) * .8;
  long double cy = yp * (MANDEL_Y_MAX - MANDEL_Y_MIN) + MANDEL_Y_MIN;
  long double cx = xp * (MANDEL_X_MAX - MANDEL_X_MIN) + MANDEL_X_MIN;

  MANDEL_X_MAX = cx + (.5 * new_x_range);
  MANDEL_X_MIN = cx - (.5 * new_x_range);

  MANDEL_Y_MAX = cy + (.5 * new_y_range);
  MANDEL_Y_MIN = cy - (.5 * new_y_range);
}

enum { UP, DOWN, LEFT, RIGHT };

void pan(int dir) {
  int pixel_size = 10;
  long double y_delta =
      ((long double)pixel_size / W_HEIGHT) * (MANDEL_Y_MAX - MANDEL_Y_MIN);
  long double x_delta =
      ((long double)pixel_size / W_WIDTH) * (MANDEL_X_MAX - MANDEL_X_MIN);
  switch (dir) {
  case UP:
    MANDEL_Y_MAX -= y_delta;
    MANDEL_Y_MIN -= y_delta;
    break;
  case DOWN:
    MANDEL_Y_MAX += y_delta;
    MANDEL_Y_MIN += y_delta;
    break;
  case LEFT:
    MANDEL_X_MAX -= x_delta;
    MANDEL_X_MIN -= x_delta;
    break;
  case RIGHT:
    MANDEL_X_MAX += x_delta;
    MANDEL_X_MIN += x_delta;
    break;
  default:
    return;
  }
}
