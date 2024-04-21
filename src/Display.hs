module Display (runner) where

-- See
-- https://hackage.haskell.org/package/vty-6.2/docs/Graphics-Vty.html
-- https://hackage.haskell.org/package/vty-6.2/docs/Graphics-Vty-Image.html#t:Image

import Graphics.Vty
import Graphics.Vty.CrossPlatform (mkVty)

-- import Rope
import qualified EditorState as Es
import Data.Maybe (fromMaybe)

runner :: IO ()
runner = do
  vty <- mkVty defaultConfig
  (termWidth, termHeight) <- displayBounds (outputIface vty)
  let editor = Es.freshEditor termWidth termHeight
  setWindowTitle vty "ysue"
  theLoop editor vty
  putStrLn "Thanks for using ysue! Next time you should use Emacs instead, just so you know..."

theLoop :: Es.EditorState -> Vty -> IO ()
theLoop Es.EditorState { Es.terminate = True } vty = shutdown vty
theLoop e@Es.EditorState { Es.refresh = True } vty = do refresh vty; theLoop e { Es.refresh = False } vty
theLoop editor vty = do
  displayEditor vty editor
  e <- nextEvent vty
  if e == EvKey (KChar 'l') [MCtrl]
    then do refresh vty; theLoop editor vty
    else do
    nextState <- Es.editorInterpret (Es.clearFlash editor) e
    theLoop nextState vty

displayEditor :: Vty -> Es.EditorState -> IO ()
displayEditor vty e = do
  (tw, _) <- displayBounds (outputIface vty)
  let (pointRow, pointCol, lns) = Es.toScreenMatrix e
      strings = map (string (defAttr `withForeColor` white)) lns
      -- img = foldl vertJoin emptyImage strings
      img = foldl1 vertJoin strings
      footer = string (defAttr `withStyle` reverseVideo) (statusString pointRow pointCol e) in
    do
      update vty $ picForImage $ img <-> footer <-> string (defAttr `withForeColor` white) (miniBuffer tw e)
      if Es.mode e == Es.Insert
        then do refresh vty
        else do return ()
      setCursorPos (outputIface vty) pointCol pointRow
      showCursor (outputIface vty)

miniBuffer :: Int -> Es.EditorState -> String
miniBuffer tw e =
  (case Es.mode e of
     Es.ReadCommand cmd -> ":" ++ cmd
     _ -> "")
  ++ " "
  ++ fromMaybe (replicate tw ' ') (Es.flashMessage e)

statusString :: Int -> Int -> Es.EditorState -> String
statusString row col e =
  take (Es.termWidth e)
  "-:"
  ++ (if Es.dirty $ Es.visitingBuffer e then "**" else "--")
  ++ "  "
  ++ Es.name (Es.visitingBuffer e)
  ++ "  "
  ++ "(" ++ show (Es.currentBuffer e) ++ "/" ++ show (length $ Es.buffers e) ++ ")"
  ++ "  "
  ++ show (Es.point $ Es.visitingBuffer e)
  ++ ":("
  ++ show row
  ++ ", "
  ++ show col
  ++ ")  <"
  ++ show (Es.mode e)
  ++ ">"
  ++ " top: " ++ show (Es.screen_top $ Es.visitingBuffer e)
  ++ replicate (Es.termWidth e) ' '

-- loremRope :: Rope
-- loremRope = fromString "Sed id ligula quis est convallis tempor\nEtiam vel neque nec dui dignissim bibendum\nFusce commodo\nNulla posuere\nDonec vitae dolor\nNullam eu ante vel est convallis dignissim\n Sed diam\n Nullam tristique diam non turpis\n Nullam eu ante vel est convallis dignissim\n"

