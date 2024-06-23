module GameLogic
  ( switchPlayer,
    checkWin,
  )
where

import Board (Board)

-- Player switching
switchPlayer :: Int -> Int
switchPlayer 1 = 2
switchPlayer 2 = 1

-- Check win combinations
checkWin :: Board -> Int -> Bool
checkWin board player = any (checkDirection player) directions
  where
    -- win directions
    directions = [horizontal, vertical, diagonal1, diagonal2]
    horizontal = [(0, 0), (0, 1), (0, 2), (0, 3)]
    vertical = [(0, 0), (1, 0), (2, 0), (3, 0)]
    diagonal1 = [(0, 0), (1, 1), (2, 2), (3, 3)]
    diagonal2 = [(0, 0), (1, -1), (2, -2), (3, -3)]

    -- check all positions on the board for a win
    checkDirection :: Int -> [(Int, Int)] -> Bool
    checkDirection player direction = any (hasLine player direction) allPositions

    -- Apositions
    allPositions = [(row, col) | row <- [0 .. 5], col <- [0 .. 6]]

    -- check 4 dots line
    hasLine :: Int -> [(Int, Int)] -> (Int, Int) -> Bool
    hasLine player direction (row, col) = all (isValidAndMatches player) (map (applyDirection (row, col)) direction)

    applyDirection :: (Int, Int) -> (Int, Int) -> (Int, Int)
    applyDirection (row, col) (dRow, dCol) = (row + dRow, col + dCol)

    isValidAndMatches :: Int -> (Int, Int) -> Bool
    isValidAndMatches player (newRow, newCol) =
      isInBounds (newRow, newCol) && (board !! newRow !! newCol) == player

    -- in bounds
    isInBounds :: (Int, Int) -> Bool
    isInBounds (row, col) = row >= 0 && row < 6 && col >= 0 && col < 7
