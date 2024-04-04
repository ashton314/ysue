{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
import Test.Hspec
import Test.QuickCheck

import Rope

genLeaf :: Gen Rope
genLeaf = do
  str <- arbitrary
  return (Leaf (length str) str)

genConcat :: Gen Rope
genConcat = do
  child1 <- ropeGen
  child2 <- ropeGen
  return (concNoMerge child1 child2)

ropeGen :: Gen Rope
ropeGen = sized $ \n ->
  if n == 0
  then genLeaf
  else oneof [genLeaf, genConcat]

instance Arbitrary Rope where
  arbitrary = ropeGen

strInsAt :: String -> Int -> String -> String
strInsAt s idx i =
  let (lside, rside) = splitAt idx s in
    lside ++ i ++ rside

strDelAt :: String -> Int -> String
strDelAt s idx =
  let (lside, rside) = splitAt idx s in
    lside ++ tail rside

grProp :: Rope -> Positive Int -> Positive Int -> Bool
grProp r (Positive i) (Positive l) = getRange r i l == take l (drop i (toString r))

main :: IO ()
main = hspec $ do
  describe "Checking properties for function 'balance'" $ do
    it "holds that balancing a node never changes its toString" $ do
      property $ \r -> toString r == toString (balance r)

    it "holds that balancing always ends with a balanced tree" $ do
      property $ \r -> balancedp (balance r)

  describe "Checking properties for function 'insAt'" $ do
    it "inserting is the same as a raw insert" $ do
      property $ \(r, i, s) -> strInsAt (toString r) i s == toString (insAt r i s)

    it "inserting gives me a balanced tree" $ do
      property $ \(r, i, s) -> balancedp (insAt (balance r) i s)

  describe "Checking properties for fromString" $ do
    it "works for any string" $ do
      property $ \s -> toString (fromString s) == s

  describe "Checking properties for getRange" $ do
    it "is the same as pulling out of a regular string" $ do
      property grProp
