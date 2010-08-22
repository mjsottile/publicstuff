-- | diffusion limited aggregation, take 1
--
-- mjsottile\@computer.org

import DLA.Vec3
import DLA.KDTree
import DLA.Rmonad
import DLA.Params
import DLA.ConfigurationReader
import Debug.Trace
import System.Exit
import System.Environment (getArgs)
import System.Random.Mersenne.Pure64

type DLANode = KDTreeNode Int

--
-- draw a random number and see if the particle sticks
--
sticks :: DLAParams -> DLAMonad Bool
sticks params = do
  d <- nextF 1.0
  return $ d < (stickiness params)

--
-- where does a particle start given the step size, multipliers, and current
-- size of the aggregate
--
starting_radius :: Double -> Double -> Double -> Double -> Double
starting_radius min_inner_radius curr_max_radius inner_mult step_size =
  max min_inner_radius (curr_max_radius + inner_mult * step_size)

--
-- where does a particle die given the current state of things
--
death_radius :: Double -> Double -> Double -> Double -> Double -> Double
death_radius min_inner_radius curr_max_radius inner_mult step_size outer_mult =
  (starting_radius min_inner_radius curr_max_radius inner_mult step_size) +
  outer_mult * step_size

update_params :: DLAParams -> Vec3 -> DLAParams
update_params params pos =
  DLAParams { stickiness = (stickiness params),
              death_rad = death_radius mir cmr imult ss omult,
              starting_rad = trace ((show (vecNorm pos))++" "++(show new_srad)) new_srad,
              min_inner_rad = mir,
              inner_mult = imult,
              outer_mult = omult,
              curr_max_rad = cmr,
              epsilon = (epsilon params),
              step_size = ss,
              rng_seed = (rng_seed params),
              num_particles = (num_particles params)}
  where
    cmr = max (curr_max_rad params) (vecNorm pos)
    mir = (min_inner_rad params)
    imult = (inner_mult params)
    ss = (step_size params)
    omult = (outer_mult params)
    new_srad = starting_radius mir cmr imult ss

--
-- walk a single particle
--
walk_particle :: DLAParams -> Vec3 -> DLANode -> Int
              -> DLAMonad (Maybe (DLAParams, DLANode))
walk_particle params pos kdt n = do
  -- 1. generate a random vector of length step_size
  step <- randVec (step_size params)

  -- 2. walk current position to new position using step
  let pos' = vecAdd pos step

  -- 3. compute norm of new position (used to see if it wandered too far)
  let distance = vecNorm pos'

  -- 4. check if the move from pos to pos' will collide with any known
  --    particles already part of the aggregate
  let collide = kdtCollisionDetect kdt pos pos' (epsilon params)

  -- 5. sample to see if it sticks
  doesStick <- sticks params

  -- 6. termination test : did we collide with one or more members of the
  --    aggregate, and if so, did we stick?
  let termTest = (length collide) > 0 && doesStick

  -- 7. have we walked into the death zone?
  let deathTest = (distance > (death_rad params))

  -- check termination test
  case termTest of
    -- yes!  so return the updated parameters and the kdtree with the
    -- new particle added.
    True -> return $ Just (update_params params pos', 
                           kdtAddPoints [(pos',n)] kdt)

    -- no termination... check death test
    False -> case deathTest of
               -- wandered into the zone of no return.  toss the particle,
               -- return nothing and let the caller restart a new one.
               True -> return Nothing
               
               -- still good, keep walking
               False -> walk_particle params pos' kdt n

--
-- initialize the world with a point at the origin
--
initialize_world :: DLAMonad DLANode
initialize_world = do
  return $ kdtAddPoints [(Vec3 0.0 0.0 0.0, 0)] newKDTree

-- 
-- one step : generate a new particle and walk it
--
singleStep :: DLAParams -> DLANode -> Int
           -> DLAMonad (DLAParams, DLANode)
singleStep params kdt n = do
  sv <- randVec (starting_rad params)
  newp <- walk_particle params sv kdt n
  case newp of
    Just p -> do return $ trace ("Particles so far: "++(show n)) p
    Nothing -> singleStep params kdt n

nsteps :: DLAParams -> DLANode -> Int 
       -> DLAMonad (DLAParams, DLANode)
nsteps params kdt 0 = do return $ (params,kdt)
nsteps params kdt n = do
  (params',kdt') <- singleStep params kdt ((num_particles params)+1-n)
  nsteps params' kdt' (n-1)

driver :: DLAParams -> DLAMonad (DLAParams, DLANode)
driver params = do
  t <- initialize_world
  (parms, t') <- nsteps params t (num_particles params)
  return (parms, t')	

--
-- sanity check arguments to see if we have enough
--
validateArgs :: [String] -> IO ()
validateArgs s = do 
  if (length s < 2)  then do putStrLn "Must specify config file and output file names."
                             exitFailure
                     else do return ()

--
-- main
--
main :: IO ()
main = do
  args <- getArgs
  validateArgs args
  let configFile = head args
  let outputFile = head (tail args)
  params <- readParameters configFile
  let ((params', t'), rngState) = runRmonad (driver params) 
                                            (pureMT (Prelude.fromIntegral $ rng_seed params))
  dumpKDTree t' outputFile
  return ()
