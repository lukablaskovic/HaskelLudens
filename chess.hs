-- Defining Pieces and instances
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Eq)

data Color = White | Black deriving (Show, Eq)

data Piece = Piece
  { pieceType :: PieceType,
    color :: Color
  }
  deriving (Show, Eq)