module ChessLogic (makeMove, isValidMove, switchColor) where

import Chessboard (Chessboard, pieceAt)
import ChessPieces

-- Function to switch the current player
switchColor :: Color -> Color
switchColor White = Black
switchColor Black = White

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

-- Function to validate if a move is valid for a given piece
isValidMove :: Chessboard -> (Int, Int) -> (Int, Int) -> Bool
isValidMove board (x1, y1) (x2, y2) = case pieceAt board (x1, y1) of
  Just piece -> 
    let color = pieceColor piece
    in case pieceAt board (x2, y2) of
         Just destPiece -> pieceColor destPiece /= color && isValidPieceMove piece (x1, y1) (x2, y2) board
         Nothing -> isValidPieceMove piece (x1, y1) (x2, y2) board
  Nothing -> False

-- Function to check if a move is valid for a specific piece
isValidPieceMove :: Piece -> (Int, Int) -> (Int, Int) -> Chessboard -> Bool
isValidPieceMove (Pawn color) = isValidPawnMove color
isValidPieceMove (Rook color) = isValidRookMove color
isValidPieceMove (Knight color) = isValidKnightMove color
isValidPieceMove (Bishop color) = isValidBishopMove color
isValidPieceMove (Queen color) = isValidQueenMove color
isValidPieceMove (King color) = isValidKingMove color

-- Specific piece move validations (simplified versions)
isValidPawnMove :: Color -> (Int, Int) -> (Int, Int) -> Chessboard -> Bool
isValidPawnMove color (x1, y1) (x2, y2) board = 
  let direction = if color == White then 1 else -1
      startRow = if color == White then 1 else 6
      isForwardMove = x1 == x2 && (y2 - y1 == direction || (y1 == startRow && y2 - y1 == 2 * direction))
      isCaptureMove = abs (x2 - x1) == 1 && y2 - y1 == direction && isOccupiedByOpponent color board (x2, y2)
  in isForwardMove || isCaptureMove

isValidRookMove :: Color -> (Int, Int) -> (Int, Int) -> Chessboard -> Bool
isValidRookMove color (x1, y1) (x2, y2) board = 
  (x1 == x2 || y1 == y2) && pathIsClear board (x1, y1) (x2, y2)

isValidKnightMove :: Color -> (Int, Int) -> (Int, Int) -> Chessboard -> Bool
isValidKnightMove color (x1, y1) (x2, y2) _ = 
  (abs (x2 - x1) == 2 && abs (y2 - y1) == 1) || (abs (x2 - x1) == 1 && abs (y2 - y1) == 2)

isValidBishopMove :: Color -> (Int, Int) -> (Int, Int) -> Chessboard -> Bool
isValidBishopMove color (x1, y1) (x2, y2) board = 
  abs (x2 - x1) == abs (y2 - y1) && pathIsClear board (x1, y1) (x2, y2)

isValidQueenMove :: Color -> (Int, Int) -> (Int, Int) -> Chessboard -> Bool
isValidQueenMove color (x1, y1) (x2, y2) board = 
  isValidRookMove color (x1, y1) (x2, y2) board || isValidBishopMove color (x1, y1) (x2, y2) board

isValidKingMove :: Color -> (Int, Int) -> (Int, Int) -> Chessboard -> Bool
isValidKingMove color (x1, y1) (x2, y2) _ = 
  abs (x2 - x1) <= 1 && abs (y2 - y1) <= 1

-- Helper functions
isOccupiedByOpponent :: Color -> Chessboard -> (Int, Int) -> Bool
isOccupiedByOpponent color board (x, y) = case board !! y !! x of
  Occupied piece -> pieceColor piece /= color
  _ -> False
  
pathIsClear :: Chessboard -> (Int, Int) -> (Int, Int) -> Bool
pathIsClear board (x1, y1) (x2, y2) =
  let deltaX = signum (x2 - x1)
      deltaY = signum (y2 - y1)
      path = takeWhile (/= (x2, y2)) $ tail $ zip (iterate (+ deltaX) x1) (iterate (+ deltaY) y1)
  in all (\(x, y) -> board !! y !! x == Empty) path