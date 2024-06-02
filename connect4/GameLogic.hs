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