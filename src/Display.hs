module Display (runner) where

-- See
-- https://hackage.haskell.org/package/vty-6.2/docs/Graphics-Vty.html
-- https://hackage.haskell.org/package/vty-6.2/docs/Graphics-Vty-Image.html#t:Image

import Graphics.Vty
import Graphics.Vty.CrossPlatform (mkVty)

runner :: IO ()
runner = do
    vty <- mkVty defaultConfig
    let line0 = string (defAttr `withForeColor` green) "first line"
        line1 = string (defAttr `withBackColor` blue) "second line"
        img = line0 <-> line1
        pic = picForImage img
    update vty pic
    e <- nextEvent vty
    shutdown vty
    print ("Last event was: " ++ show e)

-- runner2 :: IO ()
-- runner2 = do
  
