{-# LANGUAGE OverloadedRecordDot #-}
module EditorState
  ( BufferState(..)
  , EditorState(..)
  , EditorMode(..)
  , updateCurrentBuffer
  , freshBuffer
  , freshEditor
  , editorInterpret
  , visitingBuffer
  , pointCol
  , pointRow
  , findPoint
  , coordLoc
  , toScreenMatrix
  , addFlash
  , clearFlash
  , toLines) where

import Rope
import Graphics.Vty.Input.Events
import System.Directory
-- import Data.Maybe (fromMaybe)
-- import Data.Functor

data EditorMode
  = Insert
  | Replace
  | ReadCommand String
  | Normal
  deriving (Show, Eq)

data BufferState = BufferState
  { point :: Int
  , wantCol :: Int
  , mark :: Int
  , screen_top :: Int
  , dirty :: Bool
  , file :: Maybe String
  , name :: String
  , contents :: Rope
  }

data EditorState = EditorState
  { buffers :: [BufferState]
  , currentBuffer :: Int
  , termWidth :: Int
  , termHeight :: Int
  , mode :: EditorMode
  , terminate :: Bool
  , refresh :: Bool
  , flashMessage :: Maybe String
  }

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

scratchMessage :: String
scratchMessage = "Welcome to ysue\n\nYou know you should really be using Emacs, right?"

freshEditor :: Int -> Int -> EditorState
freshEditor tw th =
  let b1 = freshBuffer "scratch" scratchMessage in
    EditorState { buffers=[b1]
                , currentBuffer=0
                , termWidth=tw
                , termHeight=th
                , mode=Normal
                , refresh = True
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

-- strToLines :: String -> [(Int, String)]
-- strToLines str =
--   reverse $ snd $ foldl (\(pos, acc) l -> (pos + length l, (pos, l):acc)) (0, []) ls
--   where ls = map (++ "\n") $ lines str

bufferToLines :: Int -> BufferState -> [(Int, String)]
bufferToLines chars s =
  reverse $ snd $ foldl (\(pos, acc) l -> (pos + length l, (pos, l):acc)) (s.screen_top, []) ls
  where ls = map (++ "\n") $ lines $ getRange s.contents s.screen_top chars

wrapLines :: [(Int, String)] -> Int -> [(Int, String)]
wrapLines [] _ = []
wrapLines ((ls,str):xs) width
  | length str > width = (ls, take width str++"$"):wrapLines ((ls+width, drop width str):xs) width
  | otherwise = (ls, str):wrapLines xs width

toMatrix :: EditorState -> [(Int, String)]
toMatrix e =
  let lineData = bufferToLines (e.termWidth * e.termHeight) (visitingBuffer e) in
    wrapLines lineData (e.termWidth - 1)

-- editor state -> cursor row, cursor col, buffer matrix
toScreenMatrix :: EditorState -> (Int, Int, [String])
toScreenMatrix e =
  let wrapped = toMatrix e
      farAway = (visitingBuffer e).point + (e.termWidth * e.termHeight)
      padded = take (e.termHeight - 2) $ wrapped ++ repeat (farAway, "~")
      (row, col) = findPoint (visitingBuffer e).point padded in
    case mode e of
      ReadCommand cmd -> (e.termHeight - 1, 1 + length cmd, map snd padded)
      _ -> (row, col, map snd padded)
      -- _ -> (row, col, map (\l -> show (fst l) ++ ": " ++ snd l) padded)

-- go from point -> x,y position in buffer
findPoint :: Int -> [(Int, String)] -> (Int, Int)
findPoint p ls =
  let (rowNum, rowStart) = findCurrentRow p 0 0 ls in
    (rowNum, p - rowStart)

-- go from x,y position in buffer -> nearest point
coordLoc :: [(Int, String)] -> (Int, Int) -> Int
coordLoc lns (row, col) =
  let (rowStart, theRow) = lns !! row in
    -- subtract 1 because all rows should have 0 on the end
    rowStart + max 0 (min (length theRow - 1) col)

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

upLine :: EditorState -> EditorState
upLine e =
  let b = getCurrentBuffer e
      lns = toMatrix e
      farAway = (visitingBuffer e).point + (e.termWidth * e.termHeight)
      (row, _) = findCurrentRow b.point 0 0 $ lns ++ [(farAway, "~")]
      newPoint = coordLoc lns (max 0 (row - 1), b.wantCol) in
      -- newPoint = coordLoc lns (max 0 (row - 1), 0) in
    updateCurrentBuffer (\x -> x { point = newPoint }) e

downLine :: EditorState -> EditorState
downLine e =
  let b = getCurrentBuffer e
      lns = toMatrix e
      farAway = (visitingBuffer e).point + (e.termWidth * e.termHeight)
      (row, _) = findCurrentRow b.point 0 0 $ lns ++ [(farAway, "~")]
      newPoint = coordLoc lns (min (length lns - 1) (row + 1), b.wantCol) in
      -- newPoint = coordLoc lns (min (length lns - 1) (row + 1), 0) in
    updateCurrentBuffer (\x -> x { point = newPoint }) e

-- move further down in the buffer
scrollLineDown :: EditorState -> EditorState
scrollLineDown e =
  case toMatrix e of
    _:(nextStart, _):_ -> (updateCurrentBuffer (\x -> x { screen_top = nextStart }) e) { refresh = True }
    _ -> e

scrollLineUp :: EditorState -> EditorState
scrollLineUp e =
  let b = getCurrentBuffer e in
    if b.screen_top > 0
    then (updateCurrentBuffer (\x -> x { screen_top =
                                        maybe 0 (1 +) (isearchCharBack (b.screen_top - 2) '\n' b.contents)}) e)
         {refresh = True}
    else e

insertChar :: Char -> BufferUpdater
insertChar c b = b { point = b.point + 1, dirty = True, contents = insAt b.contents b.point [c] }

delChar :: BufferUpdater
delChar b@BufferState { point = 0 } = b
delChar b = b { point = b.point - 1, dirty = True, contents = delAt b.contents $ b.point - 1 }

editorInterpret :: EditorState -> Event -> IO EditorState
editorInterpret e@EditorState {mode = Normal} c = iNormal e c
editorInterpret e@EditorState {mode = ReadCommand _} c = iReadCommand e c
editorInterpret e@EditorState {mode = Insert} c = return $ iInsert e c
editorInterpret e _ = return e

addFlash :: String -> EditorState -> EditorState
addFlash msg e = e { flashMessage = Just msg }

clearFlash :: EditorState -> EditorState
clearFlash e = e { flashMessage = Nothing }

-- this is in the IO monad so we can write out
iNormal :: EditorState -> Event -> IO EditorState
iNormal e (EvKey (KChar 'l') []) =
  let (_, rowStart) = findCurrentRow (visitingBuffer e).point 0 0 (toMatrix e) in
    return $
    updateCurrentBuffer (\b -> b { point = min (len b.contents) (b.point + 1)
                                 , wantCol = max 0 (b.point + 1 - rowStart)}) e
iNormal e (EvKey (KChar 'h') []) =
  let (_, rowStart) = findCurrentRow (visitingBuffer e).point 0 0 (toMatrix e) in
    return $
    updateCurrentBuffer (\b -> b { point = max 0 (b.point - 1)
                                 , wantCol = max 0 (b.point - 1 - rowStart)}) e
iNormal e (EvKey (KChar 'j') []) =
  return $ downLine e
iNormal e (EvKey (KChar 'k') []) =
  return $ upLine e
iNormal e (EvKey (KChar 'q') []) =
  return $ e { terminate = True }
iNormal e (EvKey (KChar 'i') []) =
  return $ e { mode = Insert }
iNormal e (EvKey (KChar ':') []) =
  return $ e { mode = ReadCommand "" }
iNormal e (EvKey (KChar 'e') [MCtrl]) =
  return $ scrollLineDown e
iNormal e (EvKey (KChar 'y') [MCtrl]) =
  return $ scrollLineUp e
iNormal e k = return $ addFlash ("unknown key: " ++ show k) e

iInsert :: EditorState -> Event -> EditorState
iInsert e (EvKey (KChar c) []) = updateCurrentBuffer (insertChar c) e
iInsert e (EvKey KEnter []) = updateCurrentBuffer (insertChar '\n') e
iInsert e (EvKey KDel []) = updateCurrentBuffer delChar e
iInsert e (EvKey KBS []) = updateCurrentBuffer delChar e
iInsert e (EvKey KEsc []) = e { mode = Normal }
iInsert e k = addFlash ("unknown key: " ++ show k) e

iReadCommand :: EditorState -> Event -> IO EditorState
iReadCommand e (EvKey (KChar 'g') [MCtrl]) = return $ e { mode = Normal }
iReadCommand e@EditorState{ mode = ReadCommand cmd } (EvKey KEnter []) = iExCmd cmd e
iReadCommand e@EditorState{ mode = ReadCommand cmd } (EvKey (KChar c) []) =
  return $ e { mode = ReadCommand $ cmd ++ [c]}
iReadCommand e@EditorState{ mode = ReadCommand cmd } (EvKey KBS []) =
  return $ e { mode = ReadCommand $ take (length cmd - 1) cmd }
iReadCommand e k = return $ addFlash ("unknown key: " ++ show k) e

iExCmd :: String -> EditorState -> IO EditorState
iExCmd "q" e = return $ e { terminate = True }
iExCmd ('e':' ':fileName) e = do
  existsp <- doesFileExist fileName
  fileContents <- if existsp then readFile fileName else return ""
  let newBuff = (freshBuffer fileName fileContents) { file = Just fileName, name = fileName } in
    return $ e { buffers = newBuff:e.buffers, mode = Normal }

iExCmd "w" e =
  let b = visitingBuffer e
      txt = toString b.contents in
    case b.file of
      Just fileName -> do
        writeFile fileName txt
        return $ addFlash ("file written to " ++ fileName)
          $ (updateCurrentBuffer (\bb -> bb { dirty = False }) e) { mode = Normal }
      _ -> do
        return $ addFlash "error: no file for buffer" $ e { mode = Normal }

iExCmd ('w':' ':fileName) e =
  let txt = toString (visitingBuffer e).contents
      e'  = updateCurrentBuffer (\b -> b { file = Just fileName
                                         , name = fileName
                                         , dirty = False }) e in do
    writeFile fileName txt
    return $ addFlash ("file written to " ++ fileName) $ e' { mode = Normal }

iExCmd "k" e =
  case e.buffers of
    [] -> return $ e { buffers = [freshBuffer "scratch" "new buffer"], mode = Normal }
    [_] -> return $ addFlash "attempted to delete sole buffer" $ e { mode = Normal }
    b:bs -> return $ addFlash ("deleted buffer " ++ b.name) e { buffers = bs, mode = Normal }

iExCmd ('n':' ':buffName) e =
  return $ addFlash ("new buffer " ++ buffName) e { buffers = freshBuffer buffName "":e.buffers
                                                  , mode = Normal }

iExCmd cmd e = return $ addFlash ("bad command: " ++ cmd) $ e { mode = Normal }
