import Control.Monad (forever)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Graphics.Gloss (Display (InWindow), Picture (Text), white)
import Graphics.Gloss.Interface.IO.Simulate (ViewPort, simulateIO,)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (play, Display(InWindow), white)
import Control.Concurrent (forkIO, threadDelay)
import System.IO (hFlush, stdout)
import Chess (initialChessboard, drawChessboardSprites, makeMove, Chessboard)

main :: IO ()
main = do
  boardRef <- newIORef initialChessboard
  forkIO $ terminalInputLoop boardRef
  runProgram boardRef (InWindow "Chessboard" (500, 500) (100, 100))

-- Function to run the simulation program
runProgram :: IORef Chessboard -> Display -> IO ()
runProgram boardRef display = simulateIO display black 10 initialChessboard env2Pic (step boardRef)

-- Function to convert the chessboard to a Picture
env2Pic :: Chessboard -> IO Picture
env2Pic board = do
  sprites <- drawChessboardSprites board
  let boardPic = drawChessboard
  return $ pictures [boardPic, sprites]

-- Function to update the environment (chessboard) based on the IORef
step :: IORef Chessboard -> ViewPort -> Float -> Chessboard -> IO Chessboard
step boardRef _ _ _ = readIORef boardRef

-- Terminal input loop
terminalInputLoop :: IORef Chessboard -> IO ()
terminalInputLoop boardRef = forever $ do
  putStr "Enter command (e.g., 'e2 e4'): "
  hFlush stdout  -- Ensure the prompt is displayed immediately
  command <- getLine
  atomicWriteIORef boardRef =<< updateBoard command =<< readIORef boardRef

-- Function to update the board based on the command
updateBoard :: String -> Chessboard -> IO Chessboard
updateBoard command board = do
  let (from, to) = parseCommand command
  return $ makeMove from to board

-- Function to parse a command like "e2 e4" into coordinate pairs
parseCommand :: String -> ((Int, Int), (Int, Int))
parseCommand command =
  let [from, to] = words command
  in (parseSquare from, parseSquare to)

-- Function to parse a square like "e2" into a coordinate pair
parseSquare :: String -> (Int, Int)
parseSquare [file, rank] = (fileToInt file, rankToInt rank)
  where
    fileToInt c = fromEnum c - fromEnum 'a'
    rankToInt c = fromEnum c - fromEnum '1'
parseSquare _ = error "Invalid square"

-- Function to draw the chessboard
drawChessboard :: Picture
drawChessboard = pictures [drawSquare x y | x <- [0..7], y <- [0..7]]

-- Function to draw a single square of the chessboard
drawSquare :: Int -> Int -> Picture
drawSquare x y = translate (fromIntegral x * 50 - 175) (fromIntegral y * 50 - 175) $
                 color (if even (x + y) then darkBrown else lightBrown) $
                 rectangleSolid 50 50

-- Define custom colors
darkBrown :: Color
darkBrown = makeColorI 139 69 19 255  -- Saddle Brown

lightBrown :: Color
lightBrown = makeColorI 244 164 96 255 -- Sandy Brown
