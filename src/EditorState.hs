{-# LANGUAGE OverloadedRecordDot #-}
module EditorState (BufferState, updateCurrentBuffer, toLines) where

import Rope

data EditorMode
  = Insert
  | Replace
  | Command
  | Normal
  deriving (Show)

data BufferState = BufferState
  { point :: Int
  , mark :: Int
  , screen_top :: Int
  , dirty :: Bool
  , file :: Maybe String
  , name :: String
  , contents :: Rope
  } deriving (Show)

data EditorState = EditorState
  { buffers :: [BufferState]
  , currentBuffer :: Int
  , termWidth :: Int
  , termHeight :: Int
  , mode :: EditorMode
  } deriving (Show)

type BufferUpdater = BufferState -> BufferState

updateCurrentBuffer :: BufferUpdater -> EditorState -> EditorState
updateCurrentBuffer u es =
  es { buffers = replace es.buffers (es.currentBuffer, newBuf) }
  where newBuf = u (es.buffers !! es.currentBuffer)

replace :: [a] -> (Int, a) -> [a]
replace [] _ = []
replace (_:xs) (0,a) = a:xs
replace (x:xs) (n,a) =
  if n < 0
    then x:xs
    else x: replace xs (n-1,a)

bufferToLines :: Int -> BufferState -> [String]
bufferToLines chars s = lines $ getRange s.contents s.screen_top chars

visitingBuffer :: EditorState -> BufferState
visitingBuffer s = s.buffers !! s.currentBuffer

toLines :: EditorState -> [String]
toLines s = bufferToLines (s.termWidth * s.termHeight) (visitingBuffer s)

-- it would be really cool to have a diff function: I give it two Ropes and it gives me a patch
