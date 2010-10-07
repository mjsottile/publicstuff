#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "params.h"
#include "dla.h"

int main(int argc, const char * argv[]) {
	const char *param_filename;
	if(argc > 2) {
		fprintf(stderr, "Usage: %s [paramfile]\n", argv[0]);
		exit(1);
	}
	else if(argc == 2) {
		param_filename = argv[1];
	}
	else {
		param_filename = NULL;
	}
	params p;
	load_params(&p, param_filename);

	if (p.log_progress == 1) {
	  // print parameters
	  fprintf(stderr, "seed=%u\n", p.seed);
	  fprintf(stderr, "n=%d\n", p.n);
	  fprintf(stderr, "max_secs=%d\n", p.max_secs);
	  fprintf(stderr, "epsilon=%lf\n", p.epsilon);
	  fprintf(stderr, "step_size=%lf\n", p.step_size);
	  fprintf(stderr, "stickiness=%lf\n", p.stickiness);
	  fprintf(stderr, "min_inner_radius=%lf\n", p.min_inner_radius);	
	  fprintf(stderr, "inner_mult=%lf\n", p.inner_mult);
	  fprintf(stderr, "outer_mult=%lf\n", p.outer_mult);
	}

	// run the DLA
	run_dla(p);

	return 0;
}
