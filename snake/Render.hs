module Render (render) where

import Graphics.Gloss
import GameState
import Apple
import AppleCounter
import Config
import SnakeRender (loadSnakeSprites, renderSnake)

-- Defining the colors
color1, color2, edgeColor1, edgeColor2 :: Color
color1 = makeColorI 167 215 88 255 -- #A7D758
color2 = makeColorI 162 208 82 255 -- #A2D052
edgeColor1 = makeColorI 90 58 18 255 -- #693A12
edgeColor2 = makeColorI 75 48 17 255 -- #5E3511

-- Render the game state
render :: Picture -> [(String, Picture)] -> GameState -> Picture
render appleSprite snakeSprites gameState = pictures [gridPicture, snakePicture, applePicture, appleCounterPicture]
  where
    -- Create a picture for the snake 
    snakePicture = renderSnake snakeSprites (snake gameState)
    
    -- Create a picture for the apple
    applePicture = renderApple appleSprite (apple gameState)

    -- Create a picture for the apple counter
    appleCounterPicture = renderAppleCounter appleSprite (appleCount gameState)
    
    -- Create a picture for the grid
    gridPicture = pictures [ translate x y $ color (cellColor x y) $ rectangleSolid cellSize cellSize
                           | x <- [-fromIntegral windowWidth / 2, -fromIntegral windowWidth / 2 + cellSize .. fromIntegral windowWidth / 2 - cellSize]
                           , y <- [-fromIntegral windowHeight / 2, -fromIntegral windowHeight / 2 + cellSize .. fromIntegral windowHeight / 2 - cellSize]
                           ]

    -- Determine the color of the cell
    cellColor x y
      | isEdge x y = if isEdgeColor1 x y then edgeColor1 else edgeColor2
      | even (floor (x / cellSize) + floor (y / cellSize)) = color1
      | otherwise = color2

    isEdge :: Float -> Float -> Bool
    isEdge x y = x == -fromIntegral windowWidth / 2
              || x == fromIntegral windowWidth / 2 - cellSize
              || y == -fromIntegral windowHeight / 2
              || y == fromIntegral windowHeight / 2 - cellSize
    
    -- Determine if the edge cell should use edgeColor1 or edgeColor2
    isEdgeColor1 x y = even (floor (x / cellSize) + floor (y / cellSize))
