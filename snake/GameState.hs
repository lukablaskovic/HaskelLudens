module GameState where

import System.Random
import Apple (newApple)
import Config

-- Define the initial state of the game
data GameState = GameState
  { snake :: [(Float, Float)]   -- List of coordinates for the snake
  , direction :: (Float, Float) -- Current direction of the snake
  , apple :: (Float, Float)     -- Position of the apple
  , rng :: StdGen               -- Random number generator
  , timer :: Float              -- Timer to track time for apple respawn
  } deriving Show

-- Initial game state
initialState :: GameState
initialState = GameState
  { snake = [(0, 0), (cellSize, 0), (2 * cellSize, 0)]
  , direction = (cellSize, 0)
  , apple = fst $ newApple (mkStdGen 42)
  , rng = mkStdGen 42
  , timer = 0
  }

-- Move the snake in the current direction
moveSnake :: GameState -> [(Float, Float)]
moveSnake gameState = newHead : init (snake gameState)
  where
    (dx, dy) = direction gameState
    newHead = (\(x, y) -> (x + dx, y + dy)) (head $ snake gameState)

-- Check if the snake eats the apple
snakeEatsApple :: GameState -> Bool
snakeEatsApple gameState = head (snake gameState) == apple gameState

-- Check for snake collision with walls or itself
checkCollision :: (Float, Float) -> Bool
checkCollision (x, y) = x < -fromIntegral windowWidth / 2 + cellSize
                     || x >= fromIntegral windowWidth / 2 - cellSize
                     || y < -fromIntegral windowHeight / 2 + cellSize
                     || y >= fromIntegral windowHeight / 2 - cellSize

-- Grow the snake when it eats an apple
growSnake :: [(Float, Float)] -> [(Float, Float)]
growSnake (x:xs) = x : x : xs
growSnake [] = []
