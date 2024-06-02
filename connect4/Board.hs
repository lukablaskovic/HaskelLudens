module Board
  ( Board,
    initialBoard,
  )
where

type Board = [[Int]]

-- Initial
initialBoard :: Board
initialBoard = replicate 7 (replicate 7 0)
