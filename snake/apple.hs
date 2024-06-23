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

-- Generate a new random apple position, avoiding the border area
newApple :: StdGen -> ((Float, Float), StdGen)
newApple gen = ((alignToGrid x, alignToGrid y), newGen)
  where
    borderOffset = 2 * cellSize -- Offset to avoid the border area
    (x, gen1) = randomR (-fromIntegral windowWidth / 2 + borderOffset, fromIntegral windowWidth / 2 - borderOffset) gen
    (y, newGen) = randomR (-fromIntegral windowHeight / 2 + borderOffset, fromIntegral windowHeight / 2 - borderOffset) gen1

-- Align a coordinate to the center of the nearest grid cell
alignToGrid :: Float -> Float
alignToGrid coord = fromIntegral (round (coord / cellSize)) * cellSize

-- Render the apple
renderApple :: Picture -> (Float, Float) -> Picture
renderApple appleSprite (x, y) = translate x y $ scale (cellSize / 30) (cellSize / 30) appleSprite
