module PlayerInput ( parseCommand) where

import Chessboard
import ChessPieces

-- Function to parse a command like "e2e4" into coordinate pairs
parseCommand :: String -> Color -> Chessboard -> Maybe ((Int, Int), (Int, Int))
parseCommand command color board = 
  case command of
    [file1, rank1, file2, rank2] -> 
      (,) <$> parseSquare [file1, rank1] <*> parseSquare [file2, rank2]
    [piece, file, rank] | piece `elem` "RNBQKP" -> 
      let from = findPiecePosition piece file rank color board
      in (,) <$> from <*> parseSquare [file, rank]
    _ -> Nothing

-- Function to parse a square like "e2" into a coordinate pair
parseSquare :: String -> Maybe (Int, Int)
parseSquare [file, rank] 
  | file `elem` ['a'..'h'] && rank `elem` ['1'..'8'] = Just (fileToInt file, rankToInt rank)
  | otherwise = Nothing
  where
    fileToInt c = fromEnum c - fromEnum 'a'
    rankToInt c = fromEnum c - fromEnum '1'
parseSquare _ = Nothing

-- Function to find the position of a piece
findPiecePosition :: Char -> Char -> Char -> Color -> Chessboard -> Maybe (Int, Int)
findPiecePosition piece file rank color board = 
  let targetPiece = charToPiece piece color
      possiblePositions = [(x, y) | x <- [0..7], y <- [0..7], board !! y !! x == Occupied targetPiece]
  in if null possiblePositions then Nothing else Just (head possiblePositions)

-- Convert character to Piece type
charToPiece :: Char -> Color -> Piece
charToPiece 'R' color = Rook color
charToPiece 'N' color = Knight color
charToPiece 'B' color = Bishop color
charToPiece 'Q' color = Queen color
charToPiece 'K' color = King color
charToPiece 'P' color = Pawn color
charToPiece _ _ = error "Invalid piece"