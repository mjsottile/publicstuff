/*
 *  params.c
 *  kdtree
 *
 *  Created by Geoff Hulette on 6/9/08.
 *  Copyright 2008 __MyCompanyName__. All rights reserved.
 *
 */

#include <time.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#define ssize_t int
#include "params.h"
#include "sexp.h"
#include "sexp_ops.h"

void load_default_params(params *p) {
	p->seed = DEFAULT_SEED;
	p->n = DEFAULT_N;
	p->max_secs = DEFAULT_MAX_SECS;
	p->epsilon = DEFAULT_EPSILON;
	p->step_size = DEFAULT_STEP_SIZE;
	p->stickiness = DEFAULT_STICKINESS;
	p->min_inner_radius = DEFAULT_MIN_INNER_RADIUS;
	p->inner_mult = DEFAULT_INNER_MULT;
	p->outer_mult = DEFAULT_OUTER_MULT;
  strcpy(p->output_filename, DEFAULT_OUTPUT_FILENAME);
  p->log_progress = DEFAULT_LOG_PROGRESS;
}

void load_params(params *p, const char *filename) {
	int fd;
	char pstr[1024];
	sexp_t *sx, *param;
	sexp_iowrap_t *iow;
	
	load_default_params(p);
	
	if(filename == NULL) {
		return;
	}
	
	fd = open(filename, O_RDONLY);
	if(fd == 0) {
		fprintf(stderr, "Error opening paramfile %s\n", filename);
		exit(1);
	}
	iow = init_iowrap(fd);
	sx = read_one_sexp(iow);
	
	param = find_sexp("seed", sx);
	if(param != NULL) {
		print_sexp(pstr, 1024, param->next);
		p->seed = atoi(pstr);
	}
	
	param = find_sexp("n", sx);
	if(param != NULL) {
		print_sexp(pstr, 1024, param->next);
		p->n = atoi(pstr);
	}
	
	param = find_sexp("max_secs", sx);
	if(param != NULL) {
		print_sexp(pstr, 1024, param->next);
		p->max_secs = atoi(pstr);
	}
	
	param = find_sexp("epsilon", sx);
	if(param != NULL) {
		print_sexp(pstr, 1024, param->next);
		p->epsilon = strtod(pstr, NULL);
	}
	
	param = find_sexp("step_size", sx);
	if(param != NULL) {
		print_sexp(pstr, 1024, param->next);
		p->step_size = strtod(pstr, NULL);
	}
	
	param = find_sexp("stickiness", sx);
	if(param != NULL) {
		print_sexp(pstr, 1024, param->next);
		p->stickiness = strtod(pstr, NULL);
	}
	
	param = find_sexp("min_inner_radius", sx);
	if(param != NULL) {
		print_sexp(pstr, 1024, param->next);
		p->min_inner_radius = strtod(pstr, NULL);
	}
	
	param = find_sexp("inner_mult", sx);
	if(param != NULL) {
		print_sexp(pstr, 1024, param->next);
		p->inner_mult = strtod(pstr, NULL);
	}
	
	param = find_sexp("outer_mult", sx);
	if(param != NULL) {
		print_sexp(pstr, 1024, param->next);
		p->outer_mult = strtod(pstr, NULL);
	}
	
  param = find_sexp("log_progress", sx);
  if (param != NULL) {
    print_sexp(pstr, 1024, param->next);
    p->log_progress = atoi(pstr);
  }
  
  param = find_sexp("output_filename", sx);
  if (param != NULL) {
    print_sexp(pstr, 1024, param->next);
    strcpy(p->output_filename, pstr);
  }
  
	destroy_sexp(sx);
	destroy_iowrap(iow);
}
