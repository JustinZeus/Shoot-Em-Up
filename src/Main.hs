module Main where

import Controller
import Graphics.Gloss.Interface.IO.Game
import Model
import View

main :: IO ()
main = do
  highscores <- readFile "highscores.txt"
  let readparse txtLines = map read txtLines :: [Float]
      parse txtLines = map words (lines txtLines)
      scoreList = map readparse (parse highscores)
  print scoreList
  playIO
    (InWindow "Counter" (1000, 600) (0, 0)) -- Or FullScreen
    black -- Background color
    50 -- Frames per second
    initialState -- Initial state
    view -- View function
    input -- Event function
    step -- Step function