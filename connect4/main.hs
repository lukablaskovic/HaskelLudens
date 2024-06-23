module Main where

import Board
import GameLogic
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Connect 4!"
  gameLoop initialBoard 1

gameLoop :: Board -> Int -> IO ()
gameLoop board player = do
  printBoard board
  putStrLn $ "Player " ++ show player ++ "'s turn. Enter column (1-7):"
  col <- getLine
  let column = read col - 1
  if isValidMove board column
    then do
      let newBoard = makeMove board column player
      if checkWin newBoard player
        then printBoard newBoard >> putStrLn ("Player " ++ show player ++ " won, game over.")
        else
          if isBoardFull newBoard
            then printBoard newBoard >> putStrLn "The game is a draw!"
            else gameLoop newBoard (switchPlayer player)
    else putStrLn "Invalid move. Try again." >> gameLoop board player
