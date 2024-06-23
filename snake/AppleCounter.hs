module AppleCounter (renderAppleCounter) where

import Graphics.Gloss
import Config

textColor :: Color
textColor = makeColorI 218 72 15 255 -- #DA480F

-- Helper function to draw bold text by drawing the text multiple times with slight offsets
boldText :: String -> Picture
boldText str = pictures [translate x y $ text str | x <- offsets, y <- offsets]
  where
    offsets = [0, 1, -1]

renderAppleCounter :: Picture -> Int -> Picture
renderAppleCounter appleSprite count = translate x y $ pictures [appleIcon, translate 50 (-20) $ scale 0.2 0.2 $ color textColor . boldText $ "Jabuke: " ++ show count]
  where
    x = -fromIntegral windowWidth / 2
    y = fromIntegral windowHeight / 2 - 35
    appleIcon = translate 25 (-10) $ scale 1 1 appleSprite -- Adjust position and scale of the apple icon
