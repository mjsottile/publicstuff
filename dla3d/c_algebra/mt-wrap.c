/**
 * code to allow me to use the same mersenne twister code that
 * the haskell mersenne-random-pure64 library uses.  fortunately,
 * that library calls out to C for its work, so this is pretty easy
 * to drop into the C code.
 */
#include "mt19937-64.h"

/* global holding current random number generator state */
static mt_state_struct my_mt_random_state;

inline void setuprandom(unsigned long long seed) {
  init_genrand64(&my_mt_random_state, seed);
}

inline double randomdouble() {
  return genrand64_real2(&my_mt_random_state);
}
