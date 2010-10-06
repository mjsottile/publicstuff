module DLA.Params where

data DLAParams = DLAParams {
  stickiness    :: Double,
  death_rad     :: Double,
  starting_rad  :: Double,
  min_inner_rad :: Double,
  inner_mult    :: Double,
  outer_mult    :: Double,
  curr_max_rad  :: Double,
  epsilon       :: Double,
  step_size     :: Double,
  rng_seed      :: Int,
  num_particles :: Int
}
