#include <stdio.h>

#include "color.h"
#include "ray.h"
#include "vec3.h"

vec3_T ray_color(ray_T r) {
  vec3_T unit_direction = vec3_unit_vector(r.direction);
  double t = 0.5 * (unit_direction.y + 1.0);
  vec3_T white = (vec3_T){.x = 1.0, .y = 1.0, .z = 1.0};
  vec3_T blue = (vec3_T){.x = 0.5, .y = 0.7, .z = 1.0};
  return vec3_plus_vec3(vec3_mult_val(white, (1.0 - t)),
                        vec3_mult_val(blue, (1.0 - t)));
}

int main() {

  // Image
  const double aspect_ratio = 16.0 / 9.0;
  const int image_width = 400;
  const int image_height = (int)image_width / aspect_ratio;

  // Camera
  double viewport_height = 2.0;
  double viewport_width = aspect_ratio * viewport_height;
  double focal_length = 1.0;

  vec3_T origin = (vec3_T){.x = 0, .y = 0, .z = 0};
  vec3_T horizontal = (vec3_T){.x = viewport_width, .y = 0, .z = 0};
  vec3_T vertical = (vec3_T){.x = 0, .y = viewport_height, .z = 0};
  /* vec3_T lower_left_corner = origin - horizontal / 2 - vertical / 2 - */
  /*                            (vec3_T){.x = 0, .y = 0, .z = focal_length}; */

  vec3_T lower_left_corner = vec3_subt_vec3(
      vec3_subt_vec3(vec3_subt_vec3(origin, vec3_divi_val(horizontal, 2)),
                     vec3_divi_val(vertical, 2)),
      (vec3_T){.x = 0, .y = 0, .z = focal_length});

  printf("P3 %d %d\n255\n", image_width, image_height);

  for (int j = image_height - 1; j >= 0; --j) {
    for (int i = 0; i < image_width; ++i) {
      double u = (double)i / (image_width - 1);
      double v = (double)j / (image_height - 1);
      vec3_T d = vec3_subt_vec3(
          vec3_plus_vec3(
              vec3_plus_vec3(lower_left_corner, vec3_mult_val(horizontal, u)),
              vec3_mult_val(vertical, v)),
          origin);
      ray_T r = (ray_T){.origin = origin, .direction = d};
      vec3_T color = ray_color(r);
      write_color(stdout, color);
    }
  }

  return 0;
}
