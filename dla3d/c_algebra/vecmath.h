/*
 *  vecmath.h
 *  kdtree
 *
 *  Created by Geoff Hulette on 6/6/08.
 *  Copyright 2008 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef GEOFF_VEC_MATH_H
#define GEOFF_VEC_MATH_H

/*
 Data structure defining a point in KDT_DIM-space
 */
#define KDT_DIM (3)
typedef struct point {
	double coord[KDT_DIM];
} point;

typedef point vec;

inline double vec_fmax(double f1, double f2);
inline double vec_fmin(double f1, double f2);
inline double vec_length(vec *a);
inline void vec_neg(vec *dst, vec *v);
inline void vec_add(vec *dst, vec *a, vec *b);
inline void vec_sub(vec *dst, vec *a, vec *b);
inline double vec_dot_prod(vec *a, vec *b);
inline void vec_scalar_mult(vec *dst, vec *a, double x);
inline void vec_copy(point *dst, point *src);
inline void vec_zero(vec *dst);
inline void vec_rand_unit(vec *dst);
char *vec_str(vec *v, char *buff);

#endif
