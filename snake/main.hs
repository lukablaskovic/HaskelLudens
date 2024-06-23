module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GameState (GameState(..), initialState, checkCollision, growSnake, snakeEatsApple)
import qualified GameState as GS
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
  let window = InWindow "Snake Game" (windowWidth, windowHeight) (200, 200)
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
  | movementTimer gameState >= movementThreshold =
      if snakeEatsApple gameState then
        gameState { snake = growSnake (snake gameState), apple = fst $ newApple (rng gameState), rng = snd $ newApple (rng gameState), timer = 0, appleCount = appleCount gameState + 1, movementTimer = 0 }
      else if checkCollision (head newSnake) (tail newSnake) then
        gameState { isGameOver = True, movementTimer = 0 }
      else
        gameState { snake = newSnake, movementTimer = 0 }
  | otherwise = gameState { movementTimer = movementTimer gameState + seconds }
  where
    newSnake = GS.moveSnake gameState
    movementThreshold = 0.07

-- Handle apple respawn based on timer
handleAppleRespawn :: GameState -> GameState
handleAppleRespawn gameState
  | timer gameState >= 5 = gameState { apple = newApplePos, rng = newRng, timer = 0 }
  | otherwise = gameState
  where
    (newApplePos, newRng) = newApple (rng gameState)
