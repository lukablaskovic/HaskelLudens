module PlayerInput (terminalInputLoop) where

import Data.IORef (IORef, atomicWriteIORef, readIORef)
import System.IO (hFlush, stdout)
import Control.Monad (forever, unless)

import ChessLogic (makeMove, isValidMove, switchColor)
import Chessboard (Chessboard, pieceAt)
import ChessPieces


terminalInputLoop :: IORef Chessboard -> IORef Color -> IO ()
terminalInputLoop boardRef colorRef = forever $ do
  currentColor <- readIORef colorRef
  putStrLn $ "Enter command for " ++ show currentColor ++ " (e.g., 'e2e4' or 'pb' to print board): "
  hFlush stdout
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


showPiece :: Square -> String
showPiece (Occupied piece) = showPiece' piece
showPiece Empty = "."

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


parseCommand :: String -> Maybe ((Int, Int), (Int, Int))
parseCommand "pb" = Just ((0, 0), (7, 7)) -- Print entire board
parseCommand command
  | length command == 4 = 
      (,) <$> parseSquare (take 2 command) <*> parseSquare (drop 2 command)
  | otherwise = Nothing

parseSquare :: String -> Maybe (Int, Int)
parseSquare [file, rank]
  | file `elem` ['a'..'h'] && rank `elem` ['1'..'8'] = Just (fileToInt file, rankToInt rank)
  | otherwise = Nothing
  where
    fileToInt c = fromEnum c - fromEnum 'a'
    rankToInt c = fromEnum c - fromEnum '1'
parseSquare _ = Nothing