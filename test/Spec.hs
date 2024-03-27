{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
import Test.Hspec
import Test.QuickCheck

import Rope

genLeaf :: Gen Node
genLeaf = do
  str <- arbitrary
  return (Leaf (length str) str)

genConcat :: Gen Node
genConcat = do
  child1 <- ropeGen
  child2 <- ropeGen
  return (concNoMerge child1 child2)

ropeGen :: Gen Node
ropeGen = sized $ \n ->
  if n == 0
  then genLeaf
  else oneof [genLeaf, genConcat]

instance Arbitrary Node where
  arbitrary = ropeGen

main :: IO ()
main = hspec $ do
  describe "Checking properties for function 'balance'" $ do
    it "holds that balancing a node never changes its toString" $ do
      property $ \r -> toString r == toString (balance r)

    it "holds that balancing always ends with a balanced tree" $ do
      property $ \r -> balancedp (balance r)
