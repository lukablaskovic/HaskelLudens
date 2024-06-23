module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GameState
import Render
import Input

-- Main function to start the game
main :: IO ()
main = do
  let window = InWindow "Snake Game" (windowWidth, windowHeight) (100, 100)
      backgroundColor = black
      framesPerSecond = 10 -- Reduced FPS to make snake movement visible
  play window backgroundColor framesPerSecond initialState render handleEvent update

-- Update the game state
update :: Float -> GameState -> GameState
update _ gameState = moveSnake gameState
