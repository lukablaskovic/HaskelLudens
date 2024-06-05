import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game (play, Display(InWindow), white)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import System.IO (hFlush, stdout)
import Chess (initialChessboard, drawChessboardSprites, makeMove, Chessboard)
import Data.IORef (IORef, newIORef, atomicWriteIORef, readIORef)

main :: IO ()
main = do
    -- Create a shared IORef for the chessboard state
    boardRef <- newIORef initialChessboard

    -- Start a new thread for continuously updating the display
    forkIO $ displayUpdater boardRef

    -- Start the terminal input loop
    terminalInputLoop boardRef

-- Terminal input loop
terminalInputLoop :: IORef Chessboard -> IO ()
terminalInputLoop boardRef = forever $ do
    putStr "Enter command (e.g., 'e2 e4'): "
    hFlush stdout  -- Ensure the prompt is displayed immediately
    command <- getLine
    atomicWriteIORef boardRef =<< updateBoard command =<< readIORef boardRef

-- Function to update the board based on the command
updateBoard :: String -> Chessboard -> IO Chessboard
updateBoard command board = do
    let (from, to) = parseCommand command
    return $ makeMove from to board

-- Function to parse a command like "e2 e4" into coordinate pairs
parseCommand :: String -> ((Int, Int), (Int, Int))
parseCommand command =
    let [from, to] = words command
    in (parseSquare from, parseSquare to)

-- Function to parse a square like "e2" into a coordinate pair
parseSquare :: String -> (Int, Int)
parseSquare [file, rank] = (fileToInt file, rankToInt rank)
  where
    fileToInt c = fromEnum c - fromEnum 'a'
    rankToInt c = fromEnum c - fromEnum '1'
parseSquare _ = error "Invalid square"

-- Function to continuously update the display
displayUpdater :: IORef Chessboard -> IO ()
displayUpdater boardRef = forever $ do
    -- Update display
    updateDisplay boardRef
    putStrLn "Display updated"  -- Debug print
    -- Delay for smoother display (adjust as needed)
    threadDelay 100000

-- Function to update the display
updateDisplay :: IORef Chessboard -> IO ()
updateDisplay boardRef = do
    putStrLn "Updating display..."  -- Debug print
    currentBoard <- readIORef boardRef
    putStrLn "Read board from IORef"  -- Debug print
    chessboardSprites <- drawChessboardSprites currentBoard
    putStrLn "Drawn chessboard sprites"  -- Debug print
    let combinedPictures = chessboard <> chessboardSprites
    -- Display the updated contents in the same window
    displayInWindow combinedPictures
    putStrLn "Displayed combined pictures"  -- Debug print


-- Display the contents in the window
displayInWindow :: Picture -> IO ()
displayInWindow picture = display
    (InWindow "Chessboard" (400, 400) (400, 400)) -- Window properties
    black                                         -- Background color 
    picture                                       -- Picture to display

-- Define the chessboard pattern
chessboard :: Picture
chessboard = Pictures [drawSquare x y | x <- [0..7], y <- [0..7]]

-- Function to draw a single square of the chessboard
drawSquare :: Int -> Int -> Picture
drawSquare x y = Translate (fromIntegral x * 50 - 175) (fromIntegral y * 50 - 175) $ 
                 Color (if even (x + y) then darkBrown else lightBrown) $
                 rectangleSolid 50 50

-- Define custom colors
darkBrown :: Color
darkBrown = makeColorI 139 69 19 255  -- Saddle Brown

lightBrown :: Color
lightBrown = makeColorI 244 164 96 255 -- Sandy Brown
