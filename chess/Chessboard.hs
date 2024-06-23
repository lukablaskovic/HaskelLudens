module Chessboard (Chessboard, initialChessboard, drawChessboard, pieceAt, isEmpty) where

import Graphics.Gloss
import ChessPieces


type Row = [Square]
type Chessboard = [Row]

initialChessboard :: Chessboard
initialChessboard =
  [ [ Occupied (Rook White), Occupied (Knight White), Occupied (Bishop White), Occupied (Queen White)
    , Occupied (King White), Occupied (Bishop White), Occupied (Knight White), Occupied (Rook White) ]
  , replicate 8 (Occupied (Pawn White))
  , replicate 8 Empty
  , replicate 8 Empty
  , replicate 8 Empty
  , replicate 8 Empty
  , replicate 8 (Occupied (Pawn Black))
  , [ Occupied (Rook Black), Occupied (Knight Black), Occupied (Bishop Black), Occupied (Queen Black)
    , Occupied (King Black), Occupied (Bishop Black), Occupied (Knight Black), Occupied (Rook Black) ]
  ]


drawChessboard :: Picture
drawChessboard = pictures ([drawSquare x y | x <- [0..7], y <- [0..7]] ++ [drawLabel x y | x <- [0..7], y <- [0..7]] ++ [drawSideLabel x | x <- [0..7]])

drawSquare :: Int -> Int -> Picture
drawSquare x y = translate (fromIntegral x * 50 - 175) (fromIntegral y * 50 - 175) $
                 color (if even (x + y) then darkBrown else lightBrown) $
                 rectangleSolid 50 50


darkBrown :: Graphics.Gloss.Color
darkBrown = makeColorI 139 69 19 255

lightBrown :: Graphics.Gloss.Color
lightBrown = makeColorI 244 164 96 255



drawLabel :: Int -> Int -> Picture
drawLabel x _ = translate (fromIntegral x * 50 - 175) (-225) $ scale 0.15 0.15 $ color white $ text [toEnum (fromEnum 'a' + x)]

drawSideLabel :: Int -> Picture
drawSideLabel y = translate (-225) (fromIntegral y * 50 - 175) $ scale 0.15 0.15 $ color white $ text (show (9 - (8 - y)))


pieceAt :: Chessboard -> (Int, Int) -> Maybe Piece
pieceAt board (x, y) = case board !! y !! x of
  Occupied piece -> Just piece
  _ -> Nothing

isEmpty :: Square -> Bool
isEmpty Empty = True
isEmpty _ = False