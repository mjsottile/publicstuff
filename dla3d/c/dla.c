/*
*  dla.c
	*  kdtree
	*
	*  Created by Geoff Hulette on 6/9/08.
	*  Copyright 2008 __MyCompanyName__. All rights reserved.
	*
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "mt-wrap.h"
#include "params.h"
#include "dla.h"
#include "kdtree.h"

char buffer[512];

void log_progress(double perc_complete, double radius, double starting, int depth) {
	time_t t = time(NULL);
	struct tm *local = localtime(&t);
	strftime(buffer, 80, "%H:%M:%S", local);
	fprintf(stderr, "%s %5.1f%% complete, radius=%0.2f, start=%0.2f, depth=%d\n", buffer, perc_complete, radius, starting, depth);
}

inline double starting_radius(double min_inner_radius, double curr_max_radius, double inner_mult, double step_size) {
	return vec_fmax(min_inner_radius, curr_max_radius + inner_mult * step_size);
}

inline double death_radius(double min_inner_radius, double curr_max_radius, double inner_mult, double step_size, double outer_mult) {
	return starting_radius(min_inner_radius, curr_max_radius, inner_mult, step_size) + outer_mult * step_size;
}

inline int sticks(double stickiness) {
  return randomdouble() < stickiness;
}

inline void print_point(FILE *fp, int i, point *pt) {
	fprintf(fp, "%d ", i+1);
	fprintf(fp, "%s", vec_str(pt, buffer));
	fprintf(fp, "\n");
}

void run_dla(params p) {
	time_t start, now;
	FILE *fp;

	fp = fopen(p.output_filename, "w");
	if (fp == NULL) {
		fprintf(stderr,"ERROR: could not open output file '%s'\n",
			p.output_filename);
		exit(EXIT_FAILURE);
	}

	if(KDT_DIM == 2) {
		fprintf(fp,"n x y\n");
	}
	else if(KDT_DIM == 3) {
		fprintf(fp,"n x y z\n");
	}
	else {
		fprintf(stderr, "DLA not defined for KDT_DIM=%d\n", KDT_DIM);
		exit(1);
	}

	setuprandom(p.seed);

	// setup initial world conditions
	point_list *buffer = kdt_new_point_list(p.n);
	tree_node *root = kdt_new_tree();
	point *zero = (point *)malloc(sizeof(point));
	vec_zero(zero);
	kdt_add_point(root, zero);
	double curr_max_radius = 0.0f;	
	double starting_rad = starting_radius(p.min_inner_radius, curr_max_radius, p.inner_mult, p.step_size);
	double death_rad = death_radius(p.min_inner_radius, curr_max_radius, p.inner_mult, p.step_size, p.outer_mult);

	time(&start);
	time(&now);
	int i;
	for(i=0; i < p.n && (now - start) < p.max_secs; i++) {
		if(i % (p.n / 10) == 0 && p.log_progress == 1) {
			log_progress((double)i / (double)p.n * 100.0f, curr_max_radius, starting_rad, kdt_max_depth(root));
		}
		point dir, end, col;
		point *pt = (point *)malloc(sizeof(point));
		vec_rand_unit(pt);
		vec_scalar_mult(pt, pt, starting_rad);
		int walking = 1;
		while(walking) {
			vec_rand_unit(&dir);
			vec_scalar_mult(&dir, &dir, p.step_size);
			vec_add(&end, pt, &dir);
			if(kdt_collision_detect(root, pt, &end, &col, p.epsilon, buffer) && sticks(p.stickiness)) {
				vec_copy(pt, &col);
				kdt_add_point(root, pt);
				double pt_radius = vec_length(pt);
				if(pt_radius > curr_max_radius) {
					curr_max_radius = pt_radius;
					starting_rad = starting_radius(p.min_inner_radius, curr_max_radius, p.inner_mult, p.step_size);
					death_rad = death_radius(p.min_inner_radius, curr_max_radius, p.inner_mult, p.step_size, p.outer_mult);
				}
				walking = 0;
			} 
			else {
				vec_copy(pt, &end);
				if(vec_length(pt) >= death_rad) {
					// if we have moved outside the max radius, start over
					vec_rand_unit(pt);
					vec_scalar_mult(pt, pt, starting_rad);
				}
			}
		}
		print_point(fp, i, pt);
		time(&now);
	}
	if (p.log_progress == 1) {
		log_progress((double)i / (double)p.n * 100.0f, curr_max_radius, 
			starting_rad, kdt_max_depth(root));
	}
	fclose(fp);
}
