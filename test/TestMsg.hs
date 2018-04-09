module TestMsg where

import Test.Tasty.QuickCheck

import Msg

genValue :: Gen Double
genValue = choose (0, 1) `suchThat` (/= 0)

genMsg :: Int -> Double -> Gen Double -> Gen Message
genMsg timestamp total gv = Message <$> pure timestamp <*> gv <*> pure total

genChain gv = do
  n <- getSize
  stamps <- (fmap . fmap) getPositive (vectorOf n arbitrary) :: Gen [Int]
  values <- mapM (const gv) (reverse stamps)
  let chain = zipWith (\t v -> Message t v 0) stamps values
  pure (chain ++ [initialMsg])

sumChain :: Chain -> Double
sumChain chain = let len = length chain
                     scales = fromIntegral <$> [len, len - 1..1]
                     scaled = zipWith (*) scales (value <$> chain)
                  in foldr (+) 0.0 scaled

-- Chain with all values set to 1.0 should reduce to sum of indexes
prop_constOneChain :: Positive Int -> Property
prop_constOneChain (Positive n) = forAll (resize n (genChain $ pure 1.0)) $ \c ->
  scaledSum c == sum (fromIntegral <$> [n, n - 1..1])

-- can't realy produce 0 value but property should still hold
prop_constZeroChain :: Positive Int -> Property
prop_constZeroChain (Positive n) = forAll (resize n (genChain $ pure 0.0)) $ \c ->
  scaledSum c == 0.0

prop_constHalvedChain :: Positive Int -> Property
prop_constHalvedChain (Positive n) = forAll (resize n (genChain $ pure 0.5)) $ \c ->
  scaledSum c == (sum (fromIntegral <$> [n, n - 1..1]) / 2)

-- compare with epsilon ?
-- prop_constChain :: Positive Int -> Property
-- prop_constChain (Positive n) = forAll genValue $ \v ->
--   forAll (resize n (genChain $ pure v)) $ \c ->
--     scaledSum c == ((sum (fromIntegral <$> [n, n - 1..1])) * v)

