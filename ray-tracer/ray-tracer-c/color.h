#ifndef COLOR_H
#define COLOR_H

#include "vec3.h"
#include <stdio.h>

void write_color(FILE *fd, vec3_T pixel_color) {
  fprintf(fd, "%d %d %d\n", (int)(255.999 * pixel_color.x),
          (int)(255.999 * pixel_color.y), (int)(255.999 * pixel_color.z));
}

#endif
