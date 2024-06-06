module Main where

import Data.Char (intToDigit)
import Control.Monad (forever, unless)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Graphics.Gloss (Display (InWindow), Picture, black, pictures)
import Graphics.Gloss.Interface.IO.Simulate (ViewPort, simulateIO)
import Control.Concurrent (forkIO)
import System.IO (hFlush, stdout)
import ChessLogic (makeMove)
import Chessboard (initialChessboard, drawChessboard, Chessboard)
import ChessSprites (drawChessboardSprites)
import PlayerInput (parseCommand, isValidMove, pieceAt)
import ChessPieces

main :: IO ()
main = do
  boardRef <- newIORef initialChessboard
  colorRef <- newIORef White
  forkIO $ terminalInputLoop boardRef colorRef
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

terminalInputLoop :: IORef Chessboard -> IORef Color -> IO ()
terminalInputLoop boardRef colorRef = forever $ do
  currentColor <- readIORef colorRef
  putStrLn $ "Enter command for " ++ show currentColor ++ " (e.g., 'e2e4' or 'pb' to print board): "
  hFlush stdout  -- Ensure the prompt is displayed immediately
  command <- getLine
  board <- readIORef boardRef
  case command of
    "pb" -> printBoard board
    _ -> do
      let updatedBoard = case parseCommand command of
            Just (from, to) -> makeMove currentColor from to board
            Nothing -> Nothing
      case updatedBoard of
        Just newBoard -> do
          atomicWriteIORef boardRef newBoard
          atomicWriteIORef colorRef (switchColor currentColor)
        Nothing -> do
          putStrLn "Invalid command or move. Please try again."
          -- Additional prints to clarify why the move was invalid
          putStrLn "Invalid move. Reasons:"
          case parseCommand command of
            Nothing ->
              putStrLn "   - Invalid command format. Use format 'fromSquare toSquare', e.g., 'e2e4'."
            Just (from, to) -> do
              let pieceAtFrom = pieceAt board from
              case pieceAtFrom of
                Nothing ->
                  putStrLn "   - No piece at the specified 'from' square."
                Just piece -> do
                  let isValid = isValidMove board from to
                  unless isValid $
                    putStrLn "   - Invalid move according to the rules of chess."



-- Function to print the entire chessboard
printBoard :: Chessboard -> IO ()
printBoard board = do
  putStrLn "  -----------------"
  mapM_ (\(rank, row) -> putStrLn $ show rank ++ " |" ++ showRow row ++ "|") (reverse numberedRows)
  putStrLn "  -----------------"
  putStrLn "   a b c d e f g h"

  where
    numberedRows = zip [1..8] board
    showRow :: [Square] -> String
    showRow row = unwords $ map showPiece row

-- Function to show a single piece or empty square
showPiece :: Square -> String
showPiece (Occupied piece) = showPiece' piece
showPiece Empty = "."

-- Function to show a single piece
showPiece' :: Piece -> String
showPiece' (Pawn White) = "P"
showPiece' (Rook White) = "R"
showPiece' (Knight White) = "N"
showPiece' (Bishop White) = "B"
showPiece' (Queen White) = "Q"
showPiece' (King White) = "K"
showPiece' (Pawn Black) = "p"
showPiece' (Rook Black) = "r"
showPiece' (Knight Black) = "n"
showPiece' (Bishop Black) = "b"
showPiece' (Queen Black) = "q"
showPiece' (King Black) = "k"

-- Function to switch the current player
switchColor :: Color -> Color
switchColor White = Black
switchColor Black = White
