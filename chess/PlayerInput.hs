module PlayerInput (parseCommand, isValidMove, pieceAt) where

import Chessboard
import ChessPieces

-- Function to parse a command like "e2e4" into coordinate pairs
parseCommand :: String -> Maybe ((Int, Int), (Int, Int))
parseCommand "pb" = Just ((0, 0), (7, 7)) -- Print entire board
parseCommand command
  | length command == 4 = 
      (,) <$> parseSquare (take 2 command) <*> parseSquare (drop 2 command)
  | otherwise = Nothing


-- Function to parse a square like "e2" into a coordinate pair
parseSquare :: String -> Maybe (Int, Int)
parseSquare [file, rank]
  | file `elem` ['a'..'h'] && rank `elem` ['1'..'8'] = Just (fileToInt file, rankToInt rank)
  | otherwise = Nothing
  where
    fileToInt c = fromEnum c - fromEnum 'a'
    rankToInt c = fromEnum c - fromEnum '1'
parseSquare _ = Nothing

-- Function to validate if a move is valid for a given piece
isValidMove :: Chessboard -> (Int, Int) -> (Int, Int) -> Bool
isValidMove board (x1, y1) (x2, y2) = case pieceAt board (x1, y1) of
  Just piece -> isValidPieceMove piece (x1, y1) (x2, y2) board
  Nothing -> False

-- Function to get the piece at a given position
pieceAt :: Chessboard -> (Int, Int) -> Maybe Piece
pieceAt board (x, y) = case board !! y !! x of
  Occupied piece -> Just piece
  _ -> Nothing

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
  isValidRookMove color (x1, y1) (x2, y2) board || isValidBishopMove color (x1, y1) (x2, y2) board || pathIsClear board (x1, y1) (x2, y2)

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
      inBounds (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8
      path = takeWhile inBounds $ tail $ zip (iterate (+ deltaX) x1) (iterate (+ deltaY) y1)
  in all (\(x, y) -> x == x2 && y == y2 || board !! y !! x == Empty) path


