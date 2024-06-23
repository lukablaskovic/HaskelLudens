module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GameState
import Render
import Input
import Apple
import Config -- Import the Config module

-- Main function to start the game
main :: IO ()
main = do
  let window = InWindow "Snake Game" (windowWidth, windowHeight) (100, 100)
      backgroundColor = black
      framesPerSecond = 10 -- Reduced FPS to make snake movement visible
  appleSprite <- loadAppleSprite
  play window backgroundColor framesPerSecond initialState (render appleSprite) handleEvent update

-- Update the game state
update :: Float -> GameState -> GameState
update _ gameState = moveSnake gameState
