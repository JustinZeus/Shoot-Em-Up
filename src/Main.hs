module Main where

import Controller
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import View

main :: IO ()
main = do
  highscores <- readLines "highscores.txt"
  let scoreList = makeFloat highscores
  spaceShip <- loadBMP "src/img/ship1.bmp"
  alien <- loadBMP "src/img/alien.bmp"
  spaceShip2 <- loadBMP "src/img/ship2.bmp"
  asteroid <- loadBMP "src/img/astroid.bmp"
  playIO
    (InWindow "Counter" (1400, 1400) (0, 0)) -- Or FullScreen
    black -- Background color
    50 -- Frames per second
    (initialState [spaceShip, alien, spaceShip2, asteroid] scoreList) -- Initial state
    view -- View function
    input -- Event function
    step -- Step function