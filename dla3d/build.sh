#!/bin/sh
cd unoptimized-haskell; cabal clean ; cabal configure ; cabal build ; cd ..
cd optimized-haskell1; cabal clean ; cabal configure ; cabal build ; cd ..
cd optimized-haskell2; cabal clean ; cabal configure ; cabal build ; cd ..
cd optimized-haskell3; cabal clean ; cabal configure ; cabal build ; cd ..
cd optimized-haskell4; cabal clean ; cabal configure ; cabal build ; cd ..
cd optimized-haskell5; cabal clean ; cabal configure ; cabal build ; cd ..
cd optimized-haskell6; cabal clean ; cabal configure ; cabal build ; cd ..
