module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import qualified GameState as GS
import GameState (GameState(..), initialState, checkCollision, growSnake, snakeEatsApple)

import Render
import Input
import Apple
import Config
import AppleCounter
import SnakeRender (loadSnakeSprites)

import System.Random (newStdGen, mkStdGen)

-- Main function to start the game
main :: IO ()
main = do 
  let window = InWindow "Haskell Snake Game" (windowWidth, windowHeight) (200, 200)
      backgroundColor = black
      framesPerSecond = 60 
  appleSprite <- loadAppleSprite
  snakeSprites <- loadSnakeSprites
  initialGameState <- initialState
  play window backgroundColor framesPerSecond initialGameState (render appleSprite snakeSprites) handleEvent update

-- Update the game state
update :: Float -> GameState -> GameState
update seconds gameState
  | isGameOver gameState = resetGame
  | otherwise = handleAppleRespawn . handleSnakeMovement seconds $ gameState { timer = timer gameState + seconds }

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
  , movementTimer = 0
  }
  where
    rng = mkStdGen 42 

-- Handle snake movement and collision
handleSnakeMovement :: Float -> GameState -> GameState
handleSnakeMovement seconds gameState
  | movementTimer gameState >= movementThreshold = updateGameStateAfterMovement gameState
  | otherwise = gameState { movementTimer = movementTimer gameState + seconds }
  where
    movementThreshold = 0.07 -- Time in seconds between snake movements (lower is faster 😇)

-- Update the game state after movement
updateGameStateAfterMovement :: GameState -> GameState
updateGameStateAfterMovement gameState
  | snakeEatsApple gameState = handleAppleEaten gameState
  | checkCollision (head newSnake) (tail newSnake) = gameState { isGameOver = True, movementTimer = 0 }
  | otherwise = gameState { snake = newSnake, movementTimer = 0 }
  where
    newSnake = GS.moveSnake gameState

-- Handle the scenario where the snake eats an apple
handleAppleEaten :: GameState -> GameState
handleAppleEaten gameState = gameState 
  { snake = growSnake (snake gameState)
  , apple = newApplePos
  , rng = newRng
  , timer = 0
  , appleCount = appleCount gameState + 1
  , movementTimer = 0
  }
  where
    (newApplePos, newRng) = newApple (rng gameState)

-- Handle apple respawn based on timer
handleAppleRespawn :: GameState -> GameState
handleAppleRespawn gameState
  | timer gameState >= 5 = gameState { apple = newApplePos, rng = newRng, timer = 0 }
  | otherwise = gameState
  where
    (newApplePos, newRng) = newApple (rng gameState)
