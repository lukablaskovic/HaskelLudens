module ChessPieces (
    Color(..),
    Piece(..),
    Square(..)
) where

-- Data type to represent chess pieces
data Color = White | Black deriving (Eq, Show)

-- Data type to represent chess pieces
data Piece = King Color | Queen Color | Rook Color | Bishop Color | Knight Color | Pawn Color deriving (Eq, Show)

-- Data type to represent a square on the chessboard
data Square = Empty | Occupied Piece deriving (Eq, Show)