#include <stdio.h>

#include "color.h"
#include "vec3.h"

int main() {

  const int image_width = 256;
  const int image_height = 256;

  printf("P3 %d %d\n255\n", image_width, image_height);

  for (int j = image_height - 1; j >= 0; --j) {
    for (int i = 0; i < image_width; ++i) {
      double r = (double)i / (image_width - 1);
      double g = (double)j / (image_height - 1);
      double b = 0.25;
      vec3_T color = (vec3_T){.x = r, .y = g, .z = b};
      write_color(stdout, color);
    }
  }

  return 0;
}
