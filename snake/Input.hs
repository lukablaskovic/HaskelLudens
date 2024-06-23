module Input (handleEvent) where

import Graphics.Gloss.Interface.Pure.Game
import GameState

-- Handle events (e.g., key presses)
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (Char 'w') Down _ _) gameState = gameState { direction = (0, cellSize) }
handleEvent (EventKey (Char 'a') Down _ _) gameState = gameState { direction = (-cellSize, 0) }
handleEvent (EventKey (Char 's') Down _ _) gameState = gameState { direction = (0, -cellSize) }
handleEvent (EventKey (Char 'd') Down _ _) gameState = gameState { direction = (cellSize, 0) }
handleEvent _ gameState = gameState

