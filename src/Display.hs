module Display (runner) where

-- See
-- https://hackage.haskell.org/package/vty-6.2/docs/Graphics-Vty.html
-- https://hackage.haskell.org/package/vty-6.2/docs/Graphics-Vty-Image.html#t:Image

import Graphics.Vty
import Graphics.Vty.CrossPlatform (mkVty)

import Rope
import EditorState

-- runner :: IO ()
-- runner = do
--     vty <- mkVty defaultConfig
--     let line0 = string (defAttr `withForeColor` green) "first line"
--         line1 = string (defAttr `withBackColor` blue) "second line"
--         img = line0 <-> line1
--         pic = picForImage img
--     update vty pic
--     e <- nextEvent vty
--     shutdown vty
--     print ("Last event was: " ++ show e)

runner :: IO ()
runner = do
  vty <- mkVty defaultConfig
  (termWidth, termHeight) <- displayBounds (outputIface vty)
  let editor = freshEditor termWidth termHeight
  setWindowTitle vty "ysue"
  theLoop editor vty
  print "Thanks for using ysue!"

theLoop :: EditorState -> Vty -> IO ()
theLoop editor vty = do
  writeScreenLines vty (toLines editor)
  setCursorPos (outputIface vty) (pointCol editor) (pointRow editor)
  showCursor (outputIface vty)
  e <- nextEvent vty
  case e of EvKey (KChar 'q') _ -> shutdown vty
            EvKey (KChar c) _ -> theLoop (editorInterpret editor c) vty
            _ -> theLoop editor vty

writeScreenLines :: Vty -> [String] -> IO ()
writeScreenLines vty lns =
  let strings = map (string (defAttr `withForeColor` white)) lns
      img = foldl vertJoin emptyImage strings in
    update vty $ picForImage img


loremRope :: Rope
loremRope = fromString "Sed id ligula quis est convallis tempor\nEtiam vel neque nec dui dignissim bibendum\nFusce commodo\nNulla posuere\nDonec vitae dolor\nNullam eu ante vel est convallis dignissim\n Sed diam\n Nullam tristique diam non turpis\n Nullam eu ante vel est convallis dignissim\n"

