module ChessLogic (
    makeMove
) where

import ChessPieces
import Chessboard

-- Function to make a move on the chessboard
makeMove :: (Int, Int) -> (Int, Int) -> Chessboard -> Chessboard
makeMove (fromX, fromY) (toX, toY) board =
    let piece = board !! fromY !! fromX
        updatedRow row idx newSquare = take idx row ++ [newSquare] ++ drop (idx + 1) row
        updatedBoard = take toY board ++
                       [updatedRow (board !! toY) toX piece] ++
                       drop (toY + 1) board
        clearedBoard = take fromY updatedBoard ++
                       [updatedRow (updatedBoard !! fromY) fromX Empty] ++
                       drop (fromY + 1) updatedBoard
    in clearedBoard