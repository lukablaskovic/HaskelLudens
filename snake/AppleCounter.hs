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

renderAppleCounter :: Int -> Picture
renderAppleCounter count = translate x y . scale 0.15 0.15 . color textColor . boldText $ "Jabuke: " ++ show count
  where
    x = -fromIntegral windowWidth / 2 + 10
    y = fromIntegral windowHeight / 2 - 50
