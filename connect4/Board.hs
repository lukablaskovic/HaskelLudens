module Board
  ( Board,
    initialBoard,
    printBoard,
    makeMove,
    isValidMove,
    isBoardFull,
  )
where

import Data.List (transpose)

type Board = [[Int]]

-- Initial board 7x7
initialBoard :: Board
initialBoard = replicate 7 (replicate 7 0)

-- Print board
printBoard :: Board -> IO ()
printBoard board = do
  mapM_ printRow (transpose board)
  putStrLn " 1 2 3 4 5 6 7"
  where
    printRow row = putStrLn $ concatMap showCell row
    showCell 0 = " ."
    showCell 1 = " X"
    showCell 2 = " O"

-- Make a move in the specified column
makeMove :: Board -> Int -> Int -> Board
makeMove board col player =
  let (before, targetCol : after) = splitAt col board
      newCol = placeInColumn targetCol player
   in before ++ (newCol : after)
  where
    placeInColumn :: [Int] -> Int -> [Int]
    placeInColumn col player = reverse (placeDisc (reverse col) player)

    placeDisc (0 : xs) p = p : xs
    placeDisc (x : xs) p = x : placeDisc xs p
    placeDisc [] _ = [] -- In case the column is full

-- Check if a move is valid
isValidMove :: Board -> Int -> Bool
isValidMove board col
  | col < 0 || col >= length board = False -- Check if column number is in range
  | otherwise = any (== 0) (board !! col)

-- Check if the board is full
isBoardFull :: Board -> Bool
isBoardFull = all (all (/= 0))
