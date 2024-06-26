module Rope ( Rope(..)
            , len
            , conc
            , insAt
            , delAt
            , balance
            , toString
            , concNoMerge
            , height
            , balancedp
            , fromString
            , getRange
            , getPrefix
            , isearchCharBack
            , isearchCharForward
            , isearchFrom ) where

import qualified Data.Map as Map
import Data.List (elemIndex)
import Data.Maybe (isNothing)
-- import Debug.Trace

data Rope
  =  Concat Int Int Rope Rope -- Concat length height left right
  |  Leaf Int String          -- Leaf length content
  deriving (Show, Eq)

toString :: Rope -> String
toString (Leaf _ s) = s
toString (Concat _ _ lc rc) = toString lc ++ toString rc

walkLeaves :: (String -> b -> b) -> b -> Rope -> b
walkLeaves f acc (Concat _ _ lc rc) = walkLeaves f (walkLeaves f acc lc) rc
walkLeaves f acc (Leaf _ s) = f s acc

len :: Rope -> Int
len (Concat l _  _ _) = l
len (Leaf l _) = l

-- Needed for balancing---so happy I get to use this!!!
fibs :: [Int]
fibs = 1:2:zipWith (+) fibs (tail fibs)

fibsUpto :: Int -> [Int]
fibsUpto n = takeWhile (<= n) fibs

-- return the biggest Fibonacci number smaller than n
fibFloor :: Int -> Int
fibFloor = last . fibsUpto

height :: Rope -> Int
height (Concat _ h _ _) = h
height (Leaf _ _) = 0

maxHeight :: Rope -> Rope -> Int
maxHeight a b = height a `max` height b

conc :: Rope -> Rope -> Rope
conc n1 n2 =
  if balancedp n then n else balance n
  where n = concHelper n1 n2

concHelper :: Rope -> Rope -> Rope
concHelper (Leaf 0 "") n2 = n2
concHelper n1 (Leaf 0 "") = n1
concHelper n1@(Leaf l1 s1) n2@(Leaf l2 s2)
  | l1 < 32 && l2 < 32 = Leaf (l1 + l2) (s1 ++ s2)
  | otherwise = Concat (l1 + l2) 1 n1 n2
concHelper (Concat l1 d1 lc1 (Leaf rcl rcs)) (Leaf l2 s2) =
  Concat (l1 + l2) d1 lc1 (Leaf (rcl + l2) (rcs ++ s2))
concHelper n1 n2 = Concat (len n1 + len n2) (1 + maxHeight n1 n2) n1 n2

concNoMerge :: Rope -> Rope -> Rope
concNoMerge n1 n2 = Concat (len n1 + len n2) (1 + maxHeight n1 n2) n1 n2

insAt :: Rope -> Int -> String -> Rope
insAt n@(Leaf l s) idx str
  | idx == l = conc n (Leaf (length str) str)
  | idx == 0 = conc (Leaf (length str) str) n
  | idx < l =
    let (lstr, rstr) = splitAt idx s
        lstr_len = length lstr
        rstr_len = length rstr
        str_len = length str in
      conc
       (Concat (lstr_len + str_len) 1
        (Leaf lstr_len lstr)
        (Leaf str_len str))
       (Leaf rstr_len rstr)
    -- Just stick on end of string
  | otherwise = conc n (Leaf (length str) str)
insAt n@(Concat l _ lc rc) idx str
  | idx >= l = conc n (Leaf (length str) str)
  | idx == 0 = conc (Leaf (length str) str) n
  | idx <= len lc =
    let newLeft = insAt lc idx str in
      conc newLeft rc
  | otherwise = conc lc $ insAt rc (idx - len lc) str

delAt :: Rope -> Int -> Rope
delAt (Leaf 1 _) _ = Leaf 0 ""
delAt (Leaf l s) idx =
  let (lstr, rstr) = splitAt idx s in
    Concat (l-1) 1 (Leaf (length lstr) lstr) (Leaf (length rstr - 1) (tail rstr))
delAt (Concat _ _ lc rc) idx
  | idx >= len lc = conc lc (delAt rc (idx - len lc))
  | otherwise = conc (delAt lc idx) rc

getRange :: Rope -> Int -> Int -> String
getRange _ _ 0 = ""
getRange (Leaf _ s) start wantLength = take wantLength $ drop start s
getRange (Concat _ _ lc rc) start wantLength
  | len lc < start = getRange rc (start - len lc) wantLength
  | len lc >= (start + wantLength) = getRange lc start wantLength
  | otherwise = getRange lc start lcLen ++ getRange rc 0 (wantLength - lcLen)
                where lcLen = max 0 (len lc - start)

getPrefix :: Rope -> Int -> String
getPrefix (Leaf _ s) start = drop start s
getPrefix (Concat _ _ lc rc) start
  | len lc < start = getPrefix rc (start - len lc)
  | otherwise = getPrefix lc start ++ toString rc

-- really really dumb incremental search
isearchFrom :: Rope -> Int -> String -> Maybe Int
isearchFrom r start str
  | start + length str >= len r = Nothing
  | otherwise = if headMatch str asStr then Just start else isearchFrom r (start + 1) str
  where asStr = getPrefix r start

(-:) :: a -> (a -> b) -> b
x -: f = f x

isearchCharForward :: Int -> Char -> Rope -> Maybe Int
isearchCharForward start c (Leaf l s)
  | start >= l = Nothing
  | otherwise = elemIndex c (drop start s) >>= \x -> return $ start + x
isearchCharForward start c (Concat _ _ lc rc)
  | start >= len lc = isearchCharForward (start - len lc) c rc
  | otherwise = let inLeft = isearchCharForward start c lc in
                  if isNothing inLeft
                  then isearchCharForward 0 c rc >>= \x -> return $ x + len lc
                  else inLeft

isearchCharBack :: Int -> Char -> Rope -> Maybe Int
isearchCharBack start c (Leaf _ s) =
  take (start + 1) s
  -: reverse
  -: elemIndex c >>= \x -> return $ start - x
isearchCharBack  start c r@(Concat l _ lc rc)
  | start >= l = isearchCharBack (l-1) c r
  | start <= len lc = isearchCharBack start c lc
  | otherwise = let inRight = isearchCharBack (start - len lc) c rc in
                  if isNothing inRight
                  then isearchCharBack (len lc) c lc
                  else inRight >>= \x -> return $ len lc + x

-- isearchCharBack :: Rope -> Int -> Char -> Maybe Int
-- isearchCharBack (Leaf l s) start c =
--   trace ("l1: " ++ (show start) ++ " " ++ (show s)) take (start + 1) s
--   -: (\x -> trace ("head " ++ show x) x)
--   -: reverse
--   -: (\x -> trace ("reversed " ++ show x) x)
--   -: elemIndex c >>= \x -> return $ start - x
-- isearchCharBack (Concat _ _ lc rc) start c
--   | start <= len lc = trace ("b1: " ++ (show start) ++ " lc: " ++ (show lc) ++ "\nres: ") isearchCharBack lc start c
--   | otherwise = let inRight = trace ("b2: " ++ (show start) ++ " rc: " ++ (show rc) ++ "\nres: ") isearchCharBack rc (start - len lc) c in
--                   if isNothing inRight
--                   then isearchCharBack lc (len lc) c
--                   else inRight >>= \x -> return $ len lc + x

headMatch :: (Eq a) => [a] -> [a] -> Bool
headMatch [] _ = True
headMatch (_:_) [] = False
headMatch (x:xs) (y:ys)
  | x == y = headMatch xs ys
  | otherwise = False

balancedp :: Rope -> Bool
balancedp (Leaf _ _) = True
balancedp n = len n >= fibs !! height n

balance :: Rope -> Rope
balance l@(Leaf _ _) = l
balance (Concat 0 _ _ _) = Leaf 0 ""
balance n =
  let res = walkLeaves balancingAct Map.empty n in
    foldl1 (flip concNoMerge) $ map snd $ Map.toAscList res

balancingAct :: String -> Map.Map Int Rope -> Map.Map Int Rope
balancingAct str acc =
  let s = Leaf (length str) str in
    inserter acc s

inserter :: Map.Map Int Rope -> Rope -> Map.Map Int Rope
inserter m (Leaf 0 "") = m
inserter m n =
  if all (\i -> not $ Map.member i m) (fibsUpto (len n)) then
    Map.insert (fibFloor (len n)) n m
  else
    let prefixIdx = firstNonemptyKey m in
      inserter (Map.delete prefixIdx m) (concNoMerge (m Map.! prefixIdx) n)

firstNonemptyKey :: Map.Map Int a -> Int
firstNonemptyKey m = minimum $ Map.keys m

fromString :: String -> Rope
fromString s = fromStringBalanced s (length s)

fromStringBalanced :: String -> Int -> Rope
fromStringBalanced s l
  | l < 64 = let (sh, st) = splitAt 32 s in conc (Leaf (length sh) sh) (Leaf (length st) st)
  | otherwise = let head_l = div l 2
                    (sh, st) = splitAt head_l s in
                  conc (fromStringBalanced sh head_l) (fromStringBalanced st (l - head_l))

-- test string from the paper
-- paperTest :: Rope
-- paperTest = Concat 6 3 (Leaf 1 "a") (Concat 5 2 (Leaf 2 "bc") (Concat 3 1 (Leaf 1 "d") (Leaf 2 "ef")))

-- loremRope :: Rope
-- loremRope = fromString "Sed id ligula quis est convallis tempor. Etiam vel neque nec dui dignissim bibendum. Fusce commodo. Nulla posuere. Donec vitae dolor. Nullam eu ante vel est convallis dignissim.  Sed diam.  Nullam tristique diam non turpis.  Nullam eu ante vel est convallis dignissim. "

-- loremRopeSmall :: Rope
-- loremRopeSmall = fromString "Sed id ligula quis est convallis tempor.\nLorem dolor sit amet."
-- -- Sed id ligula quis est convallis tempor.nLorem dolor sit amet.

-- loremVerySmall :: Rope
-- loremVerySmall = fromString "Lorem ipsum dolor"

-- r2 :: Rope
-- r2 = Concat 8 3 (Concat 3 2 (Leaf 0 "") (Concat 3 1 (Leaf 1 "x") (Leaf 2 "<>"))) (Leaf 5 "F.1I?")

-- r3 :: Rope
-- r3 = Concat 3 2 (Concat 3 1 (Leaf 1 "a") (Leaf 2 ".b")) (Leaf 0 "")

-- getRange r3 2 1
