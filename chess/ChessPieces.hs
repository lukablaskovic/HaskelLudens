module ChessPieces (Color(..), Piece(..), Square(..), pieceColor) where


data Color = White | Black deriving (Eq, Show)
data Piece = King Color | Queen Color | Rook Color | Bishop Color | Knight Color | Pawn Color deriving (Eq, Show)
data Square = Empty | Occupied Piece deriving (Eq, Show)

pieceColor :: Piece -> Color
pieceColor (King color)   = color
pieceColor (Queen color)  = color
pieceColor (Rook color)   = color
pieceColor (Bishop color) = color
pieceColor (Knight color) = color
pieceColor (Pawn color)   = color
