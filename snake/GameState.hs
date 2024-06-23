module GameState where

import System.Random
import Apple (newApple) -- Import only newApple from Apple
import Config -- Import the Config module

-- Define the initial state of the game
data GameState = GameState
  { snake :: [(Float, Float)]   -- List of coordinates for the snake
  , direction :: (Float, Float) -- Current direction of the snake
  , apple :: (Float, Float)     -- Position of the apple
  , rng :: StdGen               -- Random number generator
  } deriving Show

-- Initial game state
initialState :: GameState
initialState = GameState
  { snake = [(0, 0), (cellSize, 0), (2 * cellSize, 0)]
  , direction = (cellSize, 0)
  , apple = fst $ newApple (mkStdGen 42)
  , rng = mkStdGen 42
  }

-- Move the snake in the current direction
moveSnake :: GameState -> GameState
moveSnake gameState
  | checkCollision newHead = gameState -- If collision, do not update the snake
  | otherwise = gameState { snake = newSnake }
  where
    (dx, dy) = direction gameState
    newHead = (\(x, y) -> (x + dx, y + dy)) (head $ snake gameState)
    newSnake = newHead : init (snake gameState)
    checkCollision (x, y) = x < -fromIntegral windowWidth / 2 + cellSize
                         || x >= fromIntegral windowWidth / 2 - cellSize
                         || y < -fromIntegral windowHeight / 2 + cellSize
                         || y >= fromIntegral windowHeight / 2 - cellSize
