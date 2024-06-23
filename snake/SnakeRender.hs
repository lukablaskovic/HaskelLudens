module SnakeRender (loadSnakeSprites, renderSnake) where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import GameState
import Config
import Data.Maybe (fromJust)

loadSnakeSprites :: IO [(String, Picture)]

loadSnakeSprites = do
  headLeft <- fromJust <$> loadJuicy "assets/head_left.png"
  headRight <- fromJust <$> loadJuicy "assets/head_right.png"
  headUp <- fromJust <$> loadJuicy "assets/head_up.png"
  headDown <- fromJust <$> loadJuicy "assets/head_down.png"
  
  bodyHorizontal <- fromJust <$> loadJuicy "assets/body_horizontal.png"
  bodyVertical <- fromJust <$> loadJuicy "assets/body_vertical.png"
  bodyTopRight <- fromJust <$> loadJuicy "assets/body_topright.png"
  bodyTopLeft <- fromJust <$> loadJuicy "assets/body_topleft.png"
  bodyBottomRight <- fromJust <$> loadJuicy "assets/body_bottomright.png"
  bodyBottomLeft <- fromJust <$> loadJuicy "assets/body_bottomleft.png"
  
  tailUp <- fromJust <$> loadJuicy "assets/tail_up.png"
  tailDown <- fromJust <$> loadJuicy "assets/tail_down.png"
  tailLeft <- fromJust <$> loadJuicy "assets/tail_left.png"
  tailRight <- fromJust <$> loadJuicy "assets/tail_right.png"
  
  return [ ("head_left", headLeft)
         , ("head_right", headRight)
         , ("head_up", headUp)
         , ("head_down", headDown)
         , ("body_horizontal", bodyHorizontal)
         , ("body_vertical", bodyVertical)
         , ("body_topright", bodyTopRight)
         , ("body_topleft", bodyTopLeft)
         , ("body_bottomright", bodyBottomRight)
         , ("body_bottomleft", bodyBottomLeft)
         , ("tail_up", tailUp)
         , ("tail_down", tailDown)
         , ("tail_left", tailLeft)
         , ("tail_right", tailRight)
         ]

-- Render the snake with the appropriate sprites
renderSnake :: [(String, Picture)] -> [(Float, Float)] -> Picture
renderSnake sprites snake = pictures $ zipWith renderSegment snake (directions snake)
  where
    renderSegment (x, y) (dir, flipX, flipY) = translate x y $ scale (cellSize / 40) (cellSize / 40) $ applyFlip flipX flipY $ fromJust (lookup dir sprites)
    applyFlip True False = scale (-1) 1
    applyFlip False True = scale 1 (-1)
    applyFlip True True = scale (-1) (-1)
    applyFlip False False = id

-- Determine the direction of each segment based on its neighbors
directions :: [(Float, Float)] -> [(String, Bool, Bool)]
directions [] = []
directions [x] = []
directions snake = headDirection : bodyDirections ++ [tailDirection]
  where
    headDirection = getHeadDirection (snake !! 0) (snake !! 1)
    bodyDirections = zipWith3 getBodyDirection (init (tail snake)) (tail (init snake)) (drop 2 snake)
    tailDirection = getTailDirection (last (init snake)) (last snake)

-- Determine the direction of the head segment based on the next segment
getHeadDirection :: (Float, Float) -> (Float, Float) -> (String, Bool, Bool)
getHeadDirection (x1, y1) (x2, y2)
  | x1 == x2 && y1 < y2 = ("head_down", False, False)
  | x1 == x2 && y1 > y2 = ("head_up", False, False)
  | x1 < x2 && y1 == y2 = ("head_left", False, False)
  | x1 > x2 && y1 == y2 = ("head_right", False, False)
  | otherwise = ("body_horizontal", False, False)

-- Determine the direction of the tail segment based on the previous segment
getTailDirection :: (Float, Float) -> (Float, Float) -> (String, Bool, Bool)
getTailDirection (x1, y1) (x2, y2)
  | x1 == x2 && y1 < y2 = ("tail_up", False, False)
  | x1 == x2 && y1 > y2 = ("tail_down", False, False)
  | x1 < x2 && y1 == y2 = ("tail_right", False, False)
  | x1 > x2 && y1 == y2 = ("tail_left", False, False)
  | otherwise = ("tail_right", False, False)

-- Determine the direction of the body segments based on the previous and next segments
getBodyDirection :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (String, Bool, Bool)
getBodyDirection (x1, y1) (x2, y2) (x3, y3)
  -- Straight horizontal and vertical segments
  | x1 == x2 && x2 == x3 = ("body_vertical", False, False)
  | y1 == y2 && y2 == y3 = ("body_horizontal", False, False)
  -- Corners
  | x1 < x2 && y2 > y3 = ("body_topleft", False, False)
  | x3 < x2 && y2 > y1 = ("body_topleft", True, True)
  | x1 < x2 && y2 < y3 = ("body_bottomleft", False, False)
  | x3 < x2 && y2 < y1 = ("body_bottomleft", True, True)
  | x1 > x2 && y2 > y3 = ("body_topright", False, False)
  | x3 > x2 && y2 > y1 = ("body_topright", True, True)
  | x1 > x2 && y2 < y3 = ("body_bottomright", False, False)
  | x3 > x2 && y2 < y1 = ("body_bottomright", True, True)
  -- Fallback to horizontal
  | otherwise = ("body_horizontal", False, False)
