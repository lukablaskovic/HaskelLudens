module ChessSprites ( drawChessboardSprites) where

import Graphics.Gloss
import ChessPieces
import Chessboard

-- Function to load images for chess pieces
loadImages :: IO [(Piece, Picture)]
loadImages = do
    let whitePieces = [ (King White, "white_king")
                      , (Queen White, "white_queen")
                      , (Rook White, "white_rook")
                      , (Bishop White, "white_bishop")
                      , (Knight White, "white_knight")
                      , (Pawn White, "white_pawn") ]
        blackPieces = [ (King Black, "black_king")
                      , (Queen Black, "black_queen")
                      , (Rook Black, "black_rook")
                      , (Bishop Black, "black_bishop")
                      , (Knight Black, "black_knight")
                      , (Pawn Black, "black_pawn") ]
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
