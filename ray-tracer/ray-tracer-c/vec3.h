#ifndef VEC3_H
#define VEC3_H

#include <math.h>
#include <stdio.h>

typedef struct vec3 {
  double x;
  double y;
  double z;
} vec3_T;

inline float vec3_x(vec3_T *v) { return v->x; };
inline float vec3_y(vec3_T *v) { return v->y; };
inline float vec3_z(vec3_T *v) { return v->z; };

inline void vec3_plus_eq(vec3_T *v, vec3_T *a) {
  v->x += a->x;
  v->y += a->y;
  v->z += a->z;
}

inline void vec3_mult_eq(vec3_T *v, float a) {
  v->x *= a;
  v->y *= a;
  v->z *= a;
}
inline void vec3_divi_eq(vec3_T *v, float a) { vec3_mult_eq(v, (1 / a)); }

inline double vec3_length_squared(vec3_T *v) {
  return v->x * v->x + v->y * v->y + v->z * v->z;
}

// Utility functions for vec3

inline double vec3_length(vec3_T *v) { return sqrt(vec3_length_squared(v)); }

inline void vec3_print(vec3_T *v) { printf("%f %f %f", v->x, v->y, v->z); }

#endif
