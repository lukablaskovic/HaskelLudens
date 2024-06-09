module PlayerInput (parseCommand) where

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