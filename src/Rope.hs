module Rope ( Node(..), conc, concAt, split, balance ) where

data Node
  =  Concat Int Node Node
  |  Leaf Int String
  deriving (Show, Eq)

len :: Node -> Int
len (Concat l _ _) = l
len (Leaf l _) = l

-- Needed for balancing---so happy I get to use this!!!
fibs :: [Int]
fibs = 0:1:zipWith (+) fibs (tail fibs)

-- TODO: add guards to make sure the leaves are not too long already
conc :: Node -> Node -> Node
conc (Leaf l1 s1) (Leaf l2 s2) = Leaf (l1 + l2) (s1 ++ s2)
conc (Concat l1 lc1 (Leaf rcl rcs)) (Leaf l2 s2) = Concat (l1 + l2) lc1 (Leaf (rcl + l2) (rcs ++ s2))
conc n1 n2 = Concat (len n1 + len n2) n1 n2

concAt :: Node -> Int -> String -> Node
concAt nd idx str = nd

split :: Node -> Int -> Node
split n idx = n

balance :: Node -> Node
balance n = n
