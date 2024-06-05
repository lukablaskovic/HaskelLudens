module Main where

import Control.Monad (forever)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Graphics.Gloss (Display (InWindow), Picture, black, pictures)
import Graphics.Gloss.Interface.IO.Simulate (ViewPort, simulateIO)
import Control.Concurrent (forkIO)
import System.IO (hFlush, stdout)
import ChessLogic (makeMove)
import Chessboard (initialChessboard, drawChessboard, Chessboard)
import ChessSprites (drawChessboardSprites)
import PlayerInput (parseCommand)
import ChessPieces (Color(..))

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

terminalInputLoop :: IORef Chessboard -> IO ()
terminalInputLoop boardRef = forever $ do
  putStr "Enter command (e.g., 'e2e4', 'Nf3'): "
  hFlush stdout  -- Ensure the prompt is displayed immediately
  command <- getLine
  updatedBoard <- updateBoard command =<< readIORef boardRef
  case updatedBoard of
    Just board -> atomicWriteIORef boardRef board
    Nothing -> putStrLn "Invalid command. Please try again."

updateBoard :: String -> Chessboard -> IO (Maybe Chessboard)
updateBoard command board = do
  -- In this example, let's assume the color is white
  let color = White
  case parseCommand command color board of
    Just (from, to) -> return $ Just (makeMove from to board)
    Nothing -> return Nothing