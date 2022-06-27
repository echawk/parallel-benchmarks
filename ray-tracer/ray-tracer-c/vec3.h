#ifndef VEC3_H
#define VEC3_H

#include <math.h>
#include <stdio.h>

typedef struct vec3 {
  double x;
  double y;
  double z;
} vec3_T;

inline double vec3_x(vec3_T *v) { return v->x; };
inline double vec3_y(vec3_T *v) { return v->y; };
inline double vec3_z(vec3_T *v) { return v->z; };

inline void vec3_plus_eq(vec3_T *v, vec3_T *a) {
  v->x += a->x;
  v->y += a->y;
  v->z += a->z;
}

inline void vec3_mult_eq(vec3_T *v, double a) {
  v->x *= a;
  v->y *= a;
  v->z *= a;
}
inline void vec3_divi_eq(vec3_T *v, double a) { vec3_mult_eq(v, (1 / a)); }

inline double vec3_length_squared(vec3_T v) {
  return v.x * v.x + v.y * v.y + v.z * v.z;
}

// Utility functions for vec3

inline double vec3_length(vec3_T v) { return sqrt(vec3_length_squared(v)); }

inline void vec3_print(vec3_T v) { printf("%f %f %f", v.x, v.y, v.z); }

inline vec3_T vec3_plus_vec3(vec3_T a, vec3_T b) {
  return (vec3_T){.x = a.x + b.x, .y = a.y + b.y, .z = a.z + b.z};
}

inline vec3_T vec3_subt_vec3(vec3_T a, vec3_T b) {
  return (vec3_T){.x = a.x - b.x, .y = a.y - b.y, .z = a.z - b.z};
}

inline vec3_T vec3_mult_vec3(vec3_T a, vec3_T b) {
  return (vec3_T){.x = a.x * b.x, .y = a.y * b.y, .z = a.z * b.z};
}

inline vec3_T vec3_mult_val(vec3_T v, double t) {
  return (vec3_T){.x = t * v.x, .y = t * v.y, .z = t * v.z};
}

inline vec3_T vec3_divi_val(vec3_T v, double t) {
  return vec3_mult_val(v, (1 / t));
}

inline double vec3_dot(vec3_T u, vec3_T v) {
  return u.x * v.x + u.y + v.y + u.z * v.z;
}

inline vec3_T vec3_cross(vec3_T u, vec3_T v) {
  return (vec3_T){.x = u.y * v.z - u.z * v.y,
                  .y = u.z * v.x - u.x * v.z,
                  .z = u.x * v.y - u.y * v.x};
}

inline vec3_T vec3_unit_vector(vec3_T v) {
  return vec3_divi_val(v, vec3_length(v));
}

#endif
