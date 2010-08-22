/*
 *  params.h
 *  kdtree
 *
 *  Created by Geoff Hulette on 6/9/08.
 *  Copyright 2008 __MyCompanyName__. All rights reserved.
 *
 */

#ifndef KDT_PARAMS_H
#define KDT_PARAMS_H

#define DEFAULT_SEED (time(NULL))
#define DEFAULT_N (10000)
#define DEFAULT_MAX_SECS (60*60*24*7)
#define DEFAULT_EPSILON (0.1f)
#define DEFAULT_STEP_SIZE (1.0f)
#define DEFAULT_STICKINESS (1.0f)
#define DEFAULT_MIN_INNER_RADIUS (3.0f)
#define DEFAULT_INNER_MULT (2.0f)
#define DEFAULT_OUTER_MULT (2.0f)
#define DEFAULT_OUTPUT_FILENAME "kdt_dla.dat"
#define DEFAULT_LOG_PROGRESS (1)

typedef struct params {
	unsigned seed;
	int n;
	int max_secs;
	float epsilon;
	float step_size;
	float stickiness;
	float min_inner_radius;
	float inner_mult;
	float outer_mult;
  char output_filename[1024];
  int log_progress;
} params;

void load_params(params *p, const char *filename);

#endif