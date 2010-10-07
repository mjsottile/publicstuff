/*
 *  vecmath.c
 *  kdtree
 *
 *  Created by Geoff Hulette on 6/6/08.
 *  Copyright 2008 __MyCompanyName__. All rights reserved.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "vecmath.h"
#include "mt-wrap.h"

inline double vec_fmax(double f1, double f2) {
	return f1 > f2 ? f1 : f2;
}

inline double vec_fmin(double f1, double f2) {
	return f1 < f2 ? f1 : f2;
}

inline double vec_length(vec *a) {
	return sqrtf(vec_dot_prod(a, a));
}

inline void vec_neg(vec *dst, vec *v) {
	int j;
	for(j=0; j < KDT_DIM; j++) {
		dst->coord[j] = -v->coord[j];
	}
}

inline void vec_add(vec *dst, vec *a, vec *b) {
	int i;
	for(i=0; i < KDT_DIM; i++) {
		dst->coord[i] = a->coord[i] + b->coord[i];
	}
}

inline void vec_sub(vec *dst, vec *a, vec *b) {
	int i;
	for(i=0; i < KDT_DIM; i++) {
		dst->coord[i] = a->coord[i] - b->coord[i];
	}
}

inline double vec_dot_prod(vec *a, vec *b) {
	int i;
	double sum = 0.0;
	for(i=0; i < KDT_DIM; i++) {
		sum += a->coord[i] * b->coord[i];
	}
	return sum;
}

inline void vec_scalar_mult(vec *dst, vec *a, double x) {
	int i;
	for(i=0; i < KDT_DIM; i++) {
		dst->coord[i] = a->coord[i] * x;
	}
}

inline void vec_copy(point *dst, point *src) {
	int i;
	for(i=0; i < KDT_DIM; i++) {
		dst->coord[i] = src->coord[i];
	}
}

inline void vec_zero(vec *dst) {
	int i;
	for(i=0; i < KDT_DIM; i++) {
		dst->coord[i] = 0.0f;
	}
}

inline void vec_rand_unit(vec *dst) {
	if(KDT_DIM == 2) {
		// my trig sucks, this is from http://www.cs.cmu.edu/~mws/rpos.html
	  double phi = randomdouble() * (2.0f * M_PI);
	  dst->coord[0] = cos(phi);
	  dst->coord[1] = sin(phi);
	}
	else if(KDT_DIM == 3) {
	  // my trig sucks, this is from http://www.cs.cmu.edu/~mws/rpos.html
	  double phi = randomdouble() * (2.0f * M_PI);
	  double z = randomdouble() * 2.0f - 1.0f;
	  double theta = asin(z);
	  dst->coord[0] = cos(theta) * cos(phi);
	  dst->coord[1] = cos(theta) * sin(phi);
	  dst->coord[2] = z;
	}
	else {
	  fprintf(stderr, "Random unit vector not defined for KDT_DIM=%d\n", KDT_DIM);
	}
}

char *vec_str(vec *v, char *buff) {
	int i=0;
	strcpy(buff, "");
	for(i=0; i < KDT_DIM; i++) {
		sprintf(buff + strlen(buff), "%lf ", v->coord[i]);
	}
	return buff;
}
