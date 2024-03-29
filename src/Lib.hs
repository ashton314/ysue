{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Lib
    ( someFunc
    , benchmarks
    ) where

import System.Random
import Rope
import Criterion.Main

someFunc :: IO ()
someFunc = putStrLn "someFunc"

benchmarks :: IO ()
benchmarks = defaultMain [
  bgroup "rope" [
      bench "100 ops" $ nfIO (runIt 100 (Leaf 1 "a") applyForRope >>= (return . toString)),
      bench "1000 ops" $ nfIO (runIt 1000 (Leaf 1 "a") applyForRope >>= (return . toString)),
      bench "10000 ops" $ nfIO (runIt 10000 (Leaf 1 "a") applyForRope >>= (return . toString)),
      bench "100000 ops" $ nfIO (runIt 100000 (Leaf 1 "a") applyForRope >>= (return . toString))
      ],
  bgroup "string" [
      bench "100 ops" $ nfIO (runIt 100 "a" applyForString),
      bench "1000 ops" $ nfIO (runIt 1000 "a" applyForString),
      bench "10000 ops" $ nfIO (runIt 10000 "a" applyForString),
      bench "100000 ops" $ nfIO (runIt 100000 "a" applyForString)
      ]
  ]

class Sized a where
  size :: a -> Int

instance Sized Node where
  size = len

instance Sized [a] where
  size = length

data Act
  = Insert Int String
  | Delete Int

runIt :: (Sized a) => Int -> a -> (a -> Act -> a) -> IO a
runIt times start app = do
  rng <- getStdGen
  return $ accumRandActs rng times start app

strInsAt :: String -> Int -> String -> String
strInsAt s idx i =
  let (lside, rside) = splitAt idx s in
    lside ++ i ++ rside

strDelAt :: String -> Int -> String
strDelAt s idx =
  let (lside, rside) = splitAt idx s in
    lside ++ tail rside

applyForString :: String -> Act -> String
applyForString s (Insert i str) = strInsAt s i str
applyForString s (Delete i) = strDelAt s i

applyForRope :: Node -> Act -> Node
applyForRope s (Insert i str) = insAt s i str
applyForRope s (Delete i) = delAt s i

-- applyForRope :: Node -> 

accumRandActs :: (Sized a) => StdGen -> Int -> a -> (a -> Act -> a) -> a
accumRandActs _ 0 x _ = x
accumRandActs gen count x applyAction =
  let (gen', act) = randActionFor x gen
      x'          = applyAction x act in
    accumRandActs gen' (count-1) x' applyAction

insertNodeBias :: Float
insertNodeBias = 0.8

randString :: StdGen -> String
randString gen = do
  take 3 $ randomRs ('a', 'z') gen

randActionFor :: (Sized a) => a -> StdGen -> (StdGen, Act)
randActionFor n gen =
  let (rand, newGen) = randomR (0, (size n) - 1) gen
      rstr = randString newGen
      (randFloat, newGen') = randomR (0.0, 1.0) newGen in
    (if (size n < 5) || (randFloat < insertNodeBias)
     then (newGen', Insert rand rstr)
     else (newGen', Delete rand))
