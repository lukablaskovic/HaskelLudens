module Input (handleEvent) where

import Graphics.Gloss.Interface.Pure.Game
import GameState
import Config -- Import the Config module

-- Handle events (e.g., key presses)
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (Char 'w') Down _ _) gameState
  | direction gameState /= (0, -cellSize) = gameState { direction = (0, cellSize) }
handleEvent (EventKey (Char 'a') Down _ _) gameState
  | direction gameState /= (cellSize, 0) = gameState { direction = (-cellSize, 0) }
handleEvent (EventKey (Char 's') Down _ _) gameState
  | direction gameState /= (0, cellSize) = gameState { direction = (0, -cellSize) }
handleEvent (EventKey (Char 'd') Down _ _) gameState
  | direction gameState /= (-cellSize, 0) = gameState { direction = (cellSize, 0) }
handleEvent _ gameState = gameState
