module ChessLogic (makeMove, isValidMove) where

import ChessPieces
import Chessboard
import PlayerInput (isValidMove)

-- Function to make a move on the chessboard if valid
makeMove :: Color -> (Int, Int) -> (Int, Int) -> Chessboard -> Maybe Chessboard
makeMove color (fromX, fromY) (toX, toY) board
  | inBounds (fromX, fromY) && inBounds (toX, toY) &&
    isValidMove board (fromX, fromY) (toX, toY) && pieceColorAt board (fromX, fromY) == Just color = 
      let piece = board !! fromY !! fromX
          updatedRow row idx newSquare = take idx row ++ [newSquare] ++ drop (idx + 1) row
          updatedBoard = take toY board ++
                         [updatedRow (board !! toY) toX piece] ++
                         drop (toY + 1) board
          clearedBoard = take fromY updatedBoard ++
                         [updatedRow (updatedBoard !! fromY) fromX Empty] ++
                         drop (fromY + 1) updatedBoard
      in Just clearedBoard
  | otherwise = Nothing

-- Check if a position is within the bounds of the chessboard
inBounds :: (Int, Int) -> Bool
inBounds (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

-- Function to get the color of the piece at a given position
pieceColorAt :: Chessboard -> (Int, Int) -> Maybe Color
pieceColorAt board (x, y) = case board !! y !! x of
  Occupied piece -> Just (pieceColor piece)
  _ -> Nothing
