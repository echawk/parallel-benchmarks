#ifndef RAY_H
#define RAY_H

#include "vec3.h"

typedef struct ray {
  vec3_T origin;
  vec3_T direction;
} ray_T;

inline vec3_T ray_at(ray_T r, double t) {
  return vec3_plus_vec3(r.origin, vec3_mult_val(r.direction, t));
}

#endif
