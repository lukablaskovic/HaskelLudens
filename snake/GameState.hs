module GameState where

-- Define the window dimensions
windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600

-- Define the size of each grid cell
cellSize :: Float
cellSize = 20

-- Define the initial state of the game
data GameState = GameState
  { snake :: [(Float, Float)]  -- List of coordinates for the snake
  , direction :: (Float, Float) -- Current direction of the snake
  } deriving Show

-- Initial game state
initialState :: GameState
initialState = GameState
  { snake = [(0, 0), (cellSize, 0), (2 * cellSize, 0)]
  , direction = (cellSize, 0)
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
    checkCollision (x, y) = x < -fromIntegral windowWidth / 2 
                         || x >= fromIntegral windowWidth / 2 
                         || y < -fromIntegral windowHeight / 2 
                         || y >= fromIntegral windowHeight / 2
