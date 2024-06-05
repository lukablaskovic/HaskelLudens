module Chess (initialChessboard, drawChessboardSprites, makeMove, Chessboard) where

import Graphics.Gloss

-- Data type to represent chess pieces
data Color = White | Black deriving (Eq, Show)-- Data type to represent chess pieces
data Piece = King Chess.Color | Queen Chess.Color | Rook Chess.Color | Bishop Chess.Color | Knight Chess.Color | Pawn Chess.Color deriving (Eq, Show)

-- Data type to represent a square on the chessboard
data Square = Empty | Occupied Piece deriving (Eq, Show)

-- Type synonym for a row of the chessboard
type Row = [Square]

-- Type synonym for the chessboard
type Chessboard = [Row]

-- Initial state of the chessboard with pieces in their starting positions
initialChessboard :: Chessboard
initialChessboard =
  [ [ Occupied (Rook Chess.White), Occupied (Knight Chess.White), Occupied (Bishop Chess.White), Occupied (Queen Chess.White)
    , Occupied (King Chess.White), Occupied (Bishop Chess.White), Occupied (Knight Chess.White), Occupied (Rook Chess.White) ]
  , replicate 8 (Occupied (Pawn Chess.White))
  , replicate 8 Empty
  , replicate 8 Empty
  , replicate 8 Empty
  , replicate 8 Empty
  , replicate 8 (Occupied (Pawn Chess.Black))
  , [ Occupied (Rook Chess.Black), Occupied (Knight Chess.Black), Occupied (Bishop Chess.Black), Occupied (Queen Chess.Black)
    , Occupied (King Chess.Black), Occupied (Bishop Chess.Black), Occupied (Knight Chess.Black), Occupied (Rook Chess.Black) ]
  ]

-- Function to load images for chess pieces
loadImages :: IO [(Piece, Picture)]
loadImages = do
    let whitePieces = [ (King Chess.White, "white_king")
                      , (Queen Chess.White, "white_queen")
                      , (Rook Chess.White, "white_rook")
                      , (Bishop Chess.White, "white_bishop")
                      , (Knight Chess.White, "white_knight")
                      , (Pawn Chess.White, "white_pawn") ]
        blackPieces = [ (King Chess.Black, "black_king")
                      , (Queen Chess.Black, "black_queen")
                      , (Rook Chess.Black, "black_rook")
                      , (Bishop Chess.Black, "black_bishop")
                      , (Knight Chess.Black, "black_knight")
                      , (Pawn Chess.Black, "black_pawn") ]
    whiteImages <- mapM (\(piece, fileName) -> (piece,) <$> loadBMP ("sprites/" ++ fileName ++ ".bmp")) whitePieces
    blackImages <- mapM (\(piece, fileName) -> (piece,) <$> loadBMP ("sprites/" ++ fileName ++ ".bmp")) blackPieces
    return $ whiteImages ++ blackImages

-- Function to draw chessboard with sprites/images
drawChessboardSprites :: Chessboard -> IO Picture
drawChessboardSprites board = do
    images <- loadImages
    let squareSize = 50  -- Adjust as needed
        xOffset = fromIntegral $ negate $ squareSize * 4 - 25
        yOffset = fromIntegral $ negate $ squareSize * 4 - 25
        spriteBoard = [ [ case sq of
                            Empty -> Blank
                            Occupied piece -> translate (fromIntegral (x * squareSize) + xOffset)
                                                        (fromIntegral (y * squareSize) + yOffset)
                                                        (snd $ head $ filter (\(p, _) -> p == piece) images)
                        | (x, sq) <- zip [0..] row ]
                      | (y, row) <- zip [0..] board ]
    return $ pictures $ concat spriteBoard

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