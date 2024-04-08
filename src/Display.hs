module Display (runner) where

-- See
-- https://hackage.haskell.org/package/vty-6.2/docs/Graphics-Vty.html
-- https://hackage.haskell.org/package/vty-6.2/docs/Graphics-Vty-Image.html#t:Image

import Graphics.Vty
import Graphics.Vty.CrossPlatform (mkVty)

import Rope
import qualified EditorState as Es

runner :: IO ()
runner = do
  vty <- mkVty defaultConfig
  (termWidth, termHeight) <- displayBounds (outputIface vty)
  let editor = Es.freshEditor termWidth termHeight
  setWindowTitle vty "ysue"
  theLoop editor vty
  print "Thanks for using ysue!"

theLoop :: Es.EditorState -> Vty -> IO ()
theLoop Es.EditorState { Es.terminate = True } vty = shutdown vty
theLoop editor vty = do
  writeScreenLines vty (Es.toLines editor) editor
  setCursorPos (outputIface vty) (Es.pointCol editor) (Es.pointRow editor)
  showCursor (outputIface vty)
  e <- nextEvent vty
  nextState <- Es.editorInterpret editor e
  theLoop nextState vty

writeScreenLines :: Vty -> [String] -> Es.EditorState -> IO ()
writeScreenLines vty lns es = do
  (_, termHeight) <- displayBounds (outputIface vty)
  let strings = map (string (defAttr `withForeColor` white)) lns
      img = foldl vertJoin emptyImage $ take (termHeight - 2) strings
      footer = string (defAttr `withStyle` reverseVideo) (statusString es) in
    update vty $ picForImage $ img <-> footer <-> string (defAttr `withForeColor` white) ""

statusString :: Es.EditorState -> String
statusString e = "foo"
  -- (if (Es.currentBuffer e))

loremRope :: Rope
loremRope = fromString "Sed id ligula quis est convallis tempor\nEtiam vel neque nec dui dignissim bibendum\nFusce commodo\nNulla posuere\nDonec vitae dolor\nNullam eu ante vel est convallis dignissim\n Sed diam\n Nullam tristique diam non turpis\n Nullam eu ante vel est convallis dignissim\n"

