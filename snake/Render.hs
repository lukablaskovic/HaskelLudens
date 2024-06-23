module Render (render) where

import Graphics.Gloss
import GameState
import Apple
import Config -- Import the Config module

-- Define the colors for the alternating squares and the edge cells
color1, color2, edgeColor :: Color
color1 = makeColorI 167 215 88 255 -- #A7D758
color2 = makeColorI 162 208 82 255 -- #A2D052
edgeColor = makeColorI 105 58 18 255 -- #693A12

-- Render the game state
render :: Picture -> GameState -> Picture
render appleSprite gameState = pictures [gridPicture, snakePicture, applePicture]
  where
    -- Create a picture for the snake
    snakePicture = color green $ pictures $ map (\(x, y) -> translate x y $ rectangleSolid cellSize cellSize) (snake gameState)
    
    -- Create a picture for the apple
    applePicture = renderApple appleSprite (apple gameState)
    
    -- Create a picture for the grid with alternating colors and edge cells in edgeColor
    gridPicture = pictures [ translate x y $ color (if isEdge x y then edgeColor else if even (floor (x / cellSize) + floor (y / cellSize)) then color1 else color2) $ rectangleSolid cellSize cellSize
                           | x <- [-fromIntegral windowWidth / 2, -fromIntegral windowWidth / 2 + cellSize .. fromIntegral windowWidth / 2 - cellSize]
                           , y <- [-fromIntegral windowHeight / 2, -fromIntegral windowHeight / 2 + cellSize .. fromIntegral windowHeight / 2 - cellSize]
                           ]

    -- Determine if a cell is on the edge
    isEdge x y = x == -fromIntegral windowWidth / 2
              || x == fromIntegral windowWidth / 2 - cellSize
              || y == -fromIntegral windowHeight / 2
              || y == fromIntegral windowHeight / 2 - cellSize
