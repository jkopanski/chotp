module TestRNG where

import Prelude                 hiding (init)
import Control.Monad.Reader
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic

import RNG

prop_deterministic seed = monadicIO $ do
  gen1 <- run (Rand <$> init seed)
  seq1 <- run (runReaderT (replicateM 100 next) gen1)
  gen2 <- run (Rand <$> init seed)
  seq2 <- run (runReaderT (replicateM 100 next) gen2)
  let results = zipWith (==) seq1 seq2
  assert $ all (== True) results
