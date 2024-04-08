{-# LANGUAGE OverloadedRecordDot #-}
module EditorState
  ( BufferState
  , EditorState(..)
  , updateCurrentBuffer
  , freshBuffer
  , freshEditor
  , editorInterpret
  , visitingBuffer
  , pointCol
  , pointRow
  , toLines) where

import Rope
import Graphics.Vty.Input.Events

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
  , terminate :: Bool
  , flashMessage :: Maybe String
  } deriving (Show)

type BufferUpdater = BufferState -> BufferState

freshBuffer :: String -> String -> BufferState
freshBuffer buf_name s = BufferState
  { point = 0
  , mark = 0
  , screen_top = 0
  , dirty = False
  , file = Nothing
  , name = buf_name
  , contents = fromString s }

freshEditor :: Int -> Int -> EditorState
freshEditor tw th =
  let b1 = freshBuffer "scratch" "Welcome to ysue" in
    EditorState { buffers=[b1]
                , currentBuffer=0
                , termWidth=tw
                , termHeight=th
                , mode=Normal
                , terminate = False
                , flashMessage = Nothing}

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

pointCol :: EditorState -> Int
pointCol e =
  b.point `rem` e.termWidth
  where b = visitingBuffer e

pointRow :: EditorState -> Int
pointRow e =
  b.point `div` e.termWidth
  where b = visitingBuffer e

bufferToLines :: Int -> BufferState -> [String]
bufferToLines chars s = lines $ getRange s.contents s.screen_top chars

visitingBuffer :: EditorState -> BufferState
visitingBuffer s = s.buffers !! s.currentBuffer

toLines :: EditorState -> [String]
toLines s = bufferToLines (s.termWidth * s.termHeight) (visitingBuffer s)

insertChar :: Char -> BufferUpdater
insertChar c b = b { point = b.point + 1, dirty = True, contents = insAt b.contents b.point [c] }

editorInterpret :: EditorState -> Event -> EditorState
editorInterpret e@EditorState {mode = Normal} c = iNormal e c
editorInterpret e@EditorState {mode = Insert} c = iInsert e c
editorInterpret e _ = e

iNormal :: EditorState -> Event -> EditorState
iNormal e (EvKey (KChar 'l') []) = updateCurrentBuffer (\b -> b { point = min (len b.contents) b.point + 1 }) e
iNormal e (EvKey (KChar 'h') []) = updateCurrentBuffer (\b -> b { point = max 0 b.point - 1 }) e
iNormal e (EvKey (KChar 'q') []) = e { terminate = True }
iNormal e (EvKey (KChar 'i') []) = e { mode = Insert }
iNormal e _ = e

iInsert :: EditorState -> Event -> EditorState
iInsert e (EvKey (KChar c) []) = updateCurrentBuffer (insertChar c) e
iInsert e (EvKey KEsc []) = e { mode = Normal }
iInsert e _ = e


-- it would be really cool to have a diff function: I give it two Ropes and it gives me a patch
