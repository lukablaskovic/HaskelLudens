module Render (render) where

import Graphics.Gloss
import GameState

-- Render the game state
render :: GameState -> Picture
render gameState = pictures [gridPicture, snakePicture, borderPicture]
  where
    -- Create a picture for the snake
    snakePicture = color green $ pictures $ map (\(x, y) -> translate x y $ rectangleSolid cellSize cellSize) (snake gameState)
    
    -- Create a picture for the grid
    gridPicture = pictures $ map colorGridLine gridLines
    colorGridLine = color (greyN 0.5)
    gridLines = [line [(x, -fromIntegral windowHeight / 2), (x, fromIntegral windowHeight / 2)] 
                  | x <- [-fromIntegral windowWidth / 2 + cellSize / 2, -fromIntegral windowWidth / 2 + cellSize / 2 + cellSize .. fromIntegral windowWidth / 2 - cellSize / 2]] ++ 
                 [line [(-fromIntegral windowWidth / 2, y), (fromIntegral windowWidth / 2, y)] 
                  | y <- [-fromIntegral windowHeight / 2 + cellSize / 2, -fromIntegral windowHeight / 2 + cellSize / 2 + cellSize .. fromIntegral windowHeight / 2 - cellSize / 2]]
    
    -- Create a picture for the borders
    borderPicture = color red $ pictures 
      [ translate (-fromIntegral windowWidth / 2 + cellSize / 2) 0 $ rectangleSolid cellSize (fromIntegral windowHeight)
      , translate (fromIntegral windowWidth / 2 - cellSize / 2) 0 $ rectangleSolid cellSize (fromIntegral windowHeight)
      , translate 0 (-fromIntegral windowHeight / 2 + cellSize / 2) $ rectangleSolid (fromIntegral windowWidth) cellSize
      , translate 0 (fromIntegral windowHeight / 2 - cellSize / 2) $ rectangleSolid (fromIntegral windowWidth) cellSize
      ]
