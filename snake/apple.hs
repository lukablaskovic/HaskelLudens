module Apple (renderApple, loadAppleSprite, newApple) where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.Random
import Config -- Import the Config module

-- Load the apple sprite
loadAppleSprite :: IO Picture
loadAppleSprite = do
  Just apple <- loadJuicy "assets/apple.png"
  return apple

-- Generate a new random apple position
newApple :: StdGen -> ((Float, Float), StdGen)
newApple gen = ((alignToGrid x, alignToGrid y), newGen)
  where
    (x, gen1) = randomR (-fromIntegral windowWidth / 2 + cellSize / 2, fromIntegral windowWidth / 2 - cellSize / 2) gen
    (y, newGen) = randomR (-fromIntegral windowHeight / 2 + cellSize / 2, fromIntegral windowHeight / 2 - cellSize / 2) gen1

-- Align a coordinate to the center of the nearest grid cell
alignToGrid :: Float -> Float
alignToGrid coord = fromIntegral (round (coord / cellSize)) * cellSize

-- Render the apple
renderApple :: Picture -> (Float, Float) -> Picture
renderApple appleSprite (x, y) = translate x y $ scale (cellSize / 40) (cellSize / 40) appleSprite
