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
        bench "10 ops" $ nfIO (runIt 10 (Leaf 1 "a") applyForRope >>= (return . toString))
      , bench "100 ops" $ nfIO (runIt 100 (Leaf 1 "a") applyForRope >>= (return . toString))
      , bench "1000 ops" $ nfIO (runIt 1000 (Leaf 1 "a") applyForRope >>= (return . toString))
      , bench "2000 ops" $ nfIO (runIt 2000 (Leaf 1 "a") applyForRope >>= (return . toString))
      , bench "3000 ops" $ nfIO (runIt 3000 (Leaf 1 "a") applyForRope >>= (return . toString))
      , bench "4000 ops" $ nfIO (runIt 4000 (Leaf 1 "a") applyForRope >>= (return . toString))
      , bench "5000 ops" $ nfIO (runIt 5000 (Leaf 1 "a") applyForRope >>= (return . toString))
      , bench "6000 ops" $ nfIO (runIt 6000 (Leaf 1 "a") applyForRope >>= (return . toString))
      , bench "7000 ops" $ nfIO (runIt 7000 (Leaf 1 "a") applyForRope >>= (return . toString))
      , bench "8000 ops" $ nfIO (runIt 8000 (Leaf 1 "a") applyForRope >>= (return . toString))
      , bench "9000 ops" $ nfIO (runIt 9000 (Leaf 1 "a") applyForRope >>= (return . toString))
      , bench "10000 ops" $ nfIO (runIt 10000 (Leaf 1 "a") applyForRope >>= (return . toString))
      -- , bench "100000 ops" $ nfIO (runIt 100000 (Leaf 1 "a") applyForRope >>= (return . toString))
      ],
  bgroup "string" [
        bench "10 ops" $ nfIO (runIt 10 "a" applyForString)
      , bench "100 ops" $ nfIO (runIt 100 "a" applyForString)
      , bench "1000 ops" $ nfIO (runIt 1000 "a" applyForString)
      , bench "2000 ops" $ nfIO (runIt 2000 "a" applyForString)
      , bench "3000 ops" $ nfIO (runIt 3000 "a" applyForString)
      , bench "4000 ops" $ nfIO (runIt 4000 "a" applyForString)
      , bench "5000 ops" $ nfIO (runIt 5000 "a" applyForString)
      , bench "6000 ops" $ nfIO (runIt 6000 "a" applyForString)
      , bench "7000 ops" $ nfIO (runIt 7000 "a" applyForString)
      , bench "8000 ops" $ nfIO (runIt 8000 "a" applyForString)
      , bench "9000 ops" $ nfIO (runIt 9000 "a" applyForString)
      , bench "10000 ops" $ nfIO (runIt 10000 "a" applyForString)
      -- , bench "100000 ops" $ nfIO (runIt 100000 "a" applyForString)
      ]
  ]

class Sized a where
  size :: a -> Int

instance Sized Rope where
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

applyForRope :: Rope -> Act -> Rope
applyForRope s (Insert i str) = insAt s i str
applyForRope s (Delete i) = delAt s i

-- applyForRope :: Rope -> 

accumRandActs :: (Sized a) => StdGen -> Int -> a -> (a -> Act -> a) -> a
accumRandActs _ 0 x _ = x
accumRandActs gen count x applyAction =
  let (gen', act) = randActionFor x gen
      x'          = applyAction x act in
    accumRandActs gen' (count-1) x' applyAction

insertRopeBias :: Float
insertRopeBias = 0.8

randString :: StdGen -> String
randString gen = do
  take 3 $ randomRs ('a', 'z') gen

randActionFor :: (Sized a) => a -> StdGen -> (StdGen, Act)
randActionFor n gen =
  let (rand, newGen) = randomR (0, (size n) - 1) gen
      rstr = randString newGen
      (randFloat, newGen') = randomR (0.0, 1.0) newGen in
    (if (size n < 5) || (randFloat < insertRopeBias)
     then (newGen', Insert rand rstr)
     else (newGen', Delete rand))
