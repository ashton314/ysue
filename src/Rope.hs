module Rope ( Node(..), conc, concAt, splitRopeAt, balance, toString ) where

import qualified Data.Map as Map
-- import Debug.Trace

data Node
  =  Concat Int Int Node Node -- Concat length height left right
  |  Leaf Int String      -- Leaf length content
  deriving (Show, Eq)

-- Since this is specific to strings, we can't make a functor out of
-- this. But this is roughly what the definition would look like:
--
-- instance Functor Node where
--   fmap f (Concat l h lc rc) = Concat l h (f lc) (f rc)

-- foldRope :: (Node -> b -> b) -> b -> Node -> b
-- foldRope f acc (Concat l h lc rc) = foldRope f acc lc

toString :: Node -> String
toString (Leaf _ s) = s
toString (Concat _ _ lc rc) = toString lc ++ toString rc

walkLeaves :: (String -> b -> b) -> b -> Node -> b
walkLeaves f acc (Concat _ _ lc rc) = walkLeaves f (walkLeaves f acc lc) rc
walkLeaves f acc (Leaf _ s) = f s acc

len :: Node -> Int
len (Concat l _  _ _) = l
len (Leaf l _) = l

-- Needed for balancing---so happy I get to use this!!!
fibs :: [Int]
fibs = 1:2:zipWith (+) fibs (tail fibs)

fibsUpto :: Int -> [Int]
fibsUpto n = takeWhile (<= n) fibs

height :: Node -> Int
height (Concat _ h _ _) = h
height (Leaf _ _) = 0

maxHeight :: Node -> Node -> Int
maxHeight a b = height a `max` height b

-- TODO: add guards to make sure the leaves are not too long already
conc :: Node -> Node -> Node
conc (Leaf l1 s1) (Leaf l2 s2) = Leaf (l1 + l2) (s1 ++ s2)
conc (Concat l1 d1 lc1 (Leaf rcl rcs)) (Leaf l2 s2) =
  Concat (l1 + l2) d1 lc1 (Leaf (rcl + l2) (rcs ++ s2))
conc n1 n2 = Concat (len n1 + len n2) (1 + maxHeight n1 n2) n1 n2

concNoMerge :: Node -> Node -> Node
concNoMerge n1 n2 = Concat (len n1 + len n2) (1 + maxHeight n1 n2) n1 n2

concAt :: Node -> Int -> String -> Node
concAt n@(Leaf l s) idx str
  | idx == l = conc n (Leaf (length str) str)
  | idx == 0 = Concat (length str + l) 1 (Leaf (length str) str) n
  | idx < l =
    let (lstr, rstr) = splitAt idx s in
      Concat (l + length str) 2
       (Concat (length lstr + length str) 1
        (Leaf (length lstr) lstr)
        (Leaf (length str) str))
       (Leaf (length rstr) rstr)
    -- Just stick on end of string... whatever
  | otherwise = Concat (l + length str) 1 n (Leaf (length str) str)
concAt n@(Concat l _ lc rc) idx str
  | idx >= l = conc n (Leaf (length str) str)
  | idx == 0 = conc (Leaf (length str) str) n
  | idx <= len lc =
    let newLeft = concAt lc idx str in
      conc newLeft rc
  | otherwise = conc lc $ concAt rc (idx - len lc) str

-- FIXME
splitRopeAt :: Node -> Int -> Node
splitRopeAt n idx = n

balancedp :: Node -> Bool
balancedp n = len n >= fibs !! (2 + height n)

-- buggy: balance $ concAt (Leaf 3 "abc") 1 "Hi there"
balance :: Node -> Node
balance l@(Leaf _ _) = l
balance n =
  let res = walkLeaves balancingAct Map.empty n in
    res Map.! firstNonemptyKey res

balancingAct :: String -> Map.Map Int Node -> Map.Map Int Node
balancingAct str acc =
  let s = Leaf (length str) str in
    inserter acc s

inserter :: Map.Map Int Node -> Node -> Map.Map Int Node
inserter m n =
  if all (\i -> not $ Map.member i m) (fibsUpto (len n)) then
    Map.insert (len n) n m
  else
    let prefixIdx = firstNonemptyKey m in
      inserter (Map.delete prefixIdx m) (concNoMerge (m Map.! prefixIdx) n)

firstNonemptyKey :: Map.Map Int a -> Int
firstNonemptyKey m = minimum $ Map.keys m

paperTest :: Node
paperTest = Concat 6 3 (Leaf 1 "a") (Concat 5 2 (Leaf 2 "bc") (Concat 3 1 (Leaf 1 "d") (Leaf 2 "ef")))

-- balanceLoop :: Node -> Map (Int, Int) Node -> Node
-- balanceLoop n _ = n
