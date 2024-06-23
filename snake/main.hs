module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GameState (GameState(..), initialState, checkCollision, growSnake, snakeEatsApple) -- Import specific functions
import qualified GameState as GS -- Qualified import for GameState to use its moveSnake
import Render
import Input
import Apple
import Config -- Import the Config module
import AppleCounter
import SnakeRender (loadSnakeSprites)
import System.Random (newStdGen, mkStdGen)

-- Main function to start the game
main :: IO ()
main = do
  let window = InWindow "Snake Game" (windowWidth, windowHeight) (100, 100)
      backgroundColor = black
      framesPerSecond = 10 -- Reduced FPS to make snake movement visible
  appleSprite <- loadAppleSprite
  snakeSprites <- loadSnakeSprites
  initialGameState <- initialState
  play window backgroundColor framesPerSecond initialGameState (render appleSprite snakeSprites) handleEvent update

-- Update the game state
update :: Float -> GameState -> GameState
update seconds gameState
  | isGameOver gameState = resetGame
  | otherwise = handleAppleRespawn . handleSnakeMovement $ gameState { timer = timer gameState + seconds }

-- Reset the game state
resetGame :: GameState
resetGame = GameState
  { snake = [(0, 0), (cellSize, 0), (2 * cellSize, 0)]
  , direction = (cellSize, 0)
  , apple = fst $ newApple rng
  , rng = snd $ newApple rng
  , timer = 0
  , appleCount = 0
  , isGameOver = False
  }
  where
    rng = mkStdGen 42  -- You can use any seed here

-- Handle snake movement and collision
handleSnakeMovement :: GameState -> GameState
handleSnakeMovement gameState
  | snakeEatsApple gameState = gameState { snake = growSnake (snake gameState), apple = fst $ newApple (rng gameState), rng = snd $ newApple (rng gameState), timer = 0, appleCount = appleCount gameState + 1 }
  | checkCollision (head newSnake) (tail newSnake) = gameState { isGameOver = True }
  | otherwise = gameState { snake = newSnake }
  where
    newSnake = GS.moveSnake gameState

-- Handle apple respawn based on timer
handleAppleRespawn :: GameState -> GameState
handleAppleRespawn gameState
  | timer gameState >= 10 = gameState { apple = newApplePos, rng = newRng, timer = 0 }
  | otherwise = gameState
  where
    (newApplePos, newRng) = newApple (rng gameState)
