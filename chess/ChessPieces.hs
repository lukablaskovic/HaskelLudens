module ChessPieces (Color(..), Piece(..), Square(..), pieceColor) where

-- Data type to represent chess pieces
data Color = White | Black deriving (Eq, Show)

-- Data type to represent chess pieces
data Piece = King Color | Queen Color | Rook Color | Bishop Color | Knight Color | Pawn Color deriving (Eq, Show)

-- Data type to represent a square on the chessboard
data Square = Empty | Occupied Piece deriving (Eq, Show)

-- Function to get the color of a piece
pieceColor :: Piece -> Color
pieceColor (King color)   = color
pieceColor (Queen color)  = color
pieceColor (Rook color)   = color
pieceColor (Bishop color) = color
pieceColor (Knight color) = color
pieceColor (Pawn color)   = color
