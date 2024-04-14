{-# LANGUAGE OverloadedRecordDot #-}
module EditorState
  ( BufferState(..)
  , EditorState(..)
  , updateCurrentBuffer
  , freshBuffer
  , freshEditor
  , editorInterpret
  , visitingBuffer
  , pointCol
  , pointRow
  , toScreenMatrix
  , toLines) where

import Rope
import Graphics.Vty.Input.Events
import Data.Maybe (fromMaybe)

data EditorMode
  = Insert
  | Replace
  | Command
  | Normal
  deriving (Show)

data BufferState = BufferState
  { point :: Int
  , wantCol :: Int
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

-- Future: add syntax highlighting
-- data Prop
--   = BackgroundColor String
--   | ForegroundColor String
--   | Info String String

-- type PropList = [Prop]

-- data PropStr
--   = Plain String
--   | Prop PropList PropStr

type BufferUpdater = BufferState -> BufferState

freshBuffer :: String -> String -> BufferState
freshBuffer buf_name s = BufferState
  { point = 0
  , wantCol = 0
  , mark = 0
  , screen_top = 0
  , dirty = False
  , file = Nothing
  , name = buf_name
  , contents = fromString s }

freshEditor :: Int -> Int -> EditorState
freshEditor tw th =
  let b1 = freshBuffer "scratch" "Welcome to ysue\n\nYou know you should really be using Emacs, right?" in
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

getCurrentBuffer :: EditorState -> BufferState
getCurrentBuffer e = e.buffers !! e.currentBuffer

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

bufferToLines :: Int -> BufferState -> [(Int, String)]
bufferToLines chars s =
  reverse $ snd $ foldl (\(pos, acc) l -> (pos + length l, (pos, l):acc)) (s.screen_top, []) ls
  where ls = lines $ getRange s.contents s.screen_top chars

wrapLines :: [(Int, String)] -> Int -> [(Int, String)]
wrapLines [] _ = []
wrapLines ((ls,str):xs) width
  | length str > width = (ls, take width str++"$"):wrapLines ((ls+width, drop width str):xs) width
  | otherwise = (ls, str):wrapLines xs width

toScreenMatrix :: EditorState -> (Int, Int, [String])
toScreenMatrix e =
  let lineData = bufferToLines (e.termWidth * e.termHeight) (visitingBuffer e)
      -- a value that is very far away from the point (for finding cursor)
      farAway = (visitingBuffer e).point + (e.termWidth * e.termHeight)
      wrapped = take (e.termHeight - 2) $ wrapLines lineData (e.termWidth - 1) ++ repeat (farAway, "~")
      (row, col) = findPoint (visitingBuffer e).point wrapped in
    (row, col, map snd wrapped)

findPoint :: Int -> [(Int, String)] -> (Int, Int)
findPoint p ls =
  let (rowNum, rowStart) = findCurrentRow p 0 0 ls in
    (rowNum, p - rowStart)

findCurrentRow :: Int -> Int -> Int -> [(Int, String)] -> (Int, Int)
findCurrentRow _ rowNum rStart [] = (rowNum, rStart)
findCurrentRow p rowNum rStart ((nextRStart,_):rst)
  | nextRStart > p = (max 0 (rowNum - 1), rStart)
  | otherwise = findCurrentRow p (rowNum + 1) nextRStart rst

-- someRows :: [(Int, String)]
-- someRows = [ (0, "hello")
--            , (6, "there")
--            , (12, "you must be new")]

visitingBuffer :: EditorState -> BufferState
visitingBuffer s = s.buffers !! s.currentBuffer

toLines :: EditorState -> [String]
toLines s = map snd $ bufferToLines (s.termWidth * s.termHeight) (visitingBuffer s)

nextLineStart :: BufferState -> Maybe Int
nextLineStart b =
  isearchFrom b.contents b.point "\n"

lineBoundaries :: Rope -> Int -> (Int, Int)
lineBoundaries r i =
  ( fromMaybe 0 $ isearchCharBack i '\n' r
  , fromMaybe (len r) $ isearchCharForward i '\n' r )

currentLineBoundaries :: BufferState -> (Int, Int)
currentLineBoundaries b = lineBoundaries b.contents b.point

previousLineBoundaries :: BufferState -> (Int, Int)
previousLineBoundaries b =
  let (thisStart, _) = currentLineBoundaries b in
    lineBoundaries b.contents (max 0 thisStart - 1)

nextLineBoundaries :: BufferState -> (Int, Int)
nextLineBoundaries b =
  let (_, thisEnd) = currentLineBoundaries b in
    lineBoundaries b.contents (min (len b.contents) thisEnd + 1)

upLine :: EditorState -> EditorState
upLine e =
  let b = getCurrentBuffer e
      (lstart, _) = currentLineBoundaries b
      (prevStart, prevEnd) = previousLineBoundaries b
      realCol = b.point - lstart
      nextPoint = if e.termWidth < realCol
                  then b.point - e.termWidth
                  else min prevEnd (prevStart + b.wantCol) in
    updateCurrentBuffer (\x -> x { point = nextPoint }) e

downLine :: EditorState -> EditorState
downLine e =
  let b = getCurrentBuffer e
      (_, lend) = currentLineBoundaries b
      (nextStart, nextEnd) = nextLineBoundaries b
      -- realCol = b.point - lstart
      nextPoint = if lend > e.termWidth + b.point
                  then b.point + e.termWidth
                  else min nextEnd (nextStart + b.wantCol) in
    updateCurrentBuffer (\x -> x { point = nextPoint }) e

insertChar :: Char -> BufferUpdater
insertChar c b = b { point = b.point + 1, dirty = True, contents = insAt b.contents b.point [c] }

delChar :: BufferUpdater
delChar b = b { point = b.point - 1, dirty = True, contents = delAt b.contents b.point }

editorInterpret :: EditorState -> Event -> IO EditorState
editorInterpret e@EditorState {mode = Normal} c = iNormal e c
editorInterpret e@EditorState {mode = Insert} c = return $ iInsert e c
editorInterpret e _ = return e

-- this is in the IO monad so we can write out
iNormal :: EditorState -> Event -> IO EditorState
iNormal e (EvKey (KChar 'l') []) =
  return $ updateCurrentBuffer (\b -> b { point = min (len b.contents) b.point + 1 }) e
iNormal e (EvKey (KChar 'h') []) =
  return $ updateCurrentBuffer (\b -> b { point = max 0 b.point - 1 }) e
iNormal e (EvKey (KChar 'j') []) =
  return $ downLine e
iNormal e (EvKey (KChar 'k') []) =
  return $ upLine e
iNormal e (EvKey (KChar 'q') []) =
  return $ e { terminate = True }
iNormal e (EvKey (KChar 'i') []) =
  return $ e { mode = Insert }
iNormal e _ = return e

iInsert :: EditorState -> Event -> EditorState
iInsert e (EvKey (KChar c) []) = updateCurrentBuffer (insertChar c) e
iInsert e (EvKey KEnter []) = updateCurrentBuffer (insertChar '\n') e
iInsert e (EvKey KDel []) = updateCurrentBuffer delChar e
iInsert e (EvKey KEsc []) = e { mode = Normal }
iInsert e _ = e


-- it would be really cool to have a diff function: I give it two Ropes and it gives me a patch
