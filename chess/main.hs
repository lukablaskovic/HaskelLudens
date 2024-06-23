module Main where

import Data.IORef (IORef, newIORef, readIORef)
import Graphics.Gloss (Display (InWindow), Picture, black, pictures)
import Graphics.Gloss.Interface.IO.Simulate (ViewPort, simulateIO)
import Control.Concurrent (forkIO)

import Chessboard (initialChessboard, drawChessboard, Chessboard)
import ChessSprites (drawChessboardSprites)
import PlayerInput (terminalInputLoop)
import ChessPieces


main :: IO ()
main = do
  boardRef <- newIORef initialChessboard
  colorRef <- newIORef White
  forkIO $ terminalInputLoop boardRef colorRef
  runProgram boardRef (InWindow "Chessboard" (500, 500) (100, 100))


runProgram :: IORef Chessboard -> Display -> IO ()
runProgram boardRef display = simulateIO display black 10 initialChessboard env2Pic (step boardRef)


env2Pic :: Chessboard -> IO Picture
env2Pic board = do
  sprites <- drawChessboardSprites board
  let boardPic = drawChessboard
  return $ pictures [boardPic, sprites]


step :: IORef Chessboard -> ViewPort -> Float -> Chessboard -> IO Chessboard
step boardRef _ _ _ = readIORef boardRef