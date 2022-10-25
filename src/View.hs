-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [backgroundPicture, scorePicture,playerPicture, bulletPicture, enemyPicture,viewBox]
              where
                scorePicture = viewScore gstate
                playerPicture = viewPlayer gstate
                bulletPicture = viewBullets gstate
                enemyPicture = viewEnemy gstate
                backgroundPicture = viewStar gstate
                

viewBox :: Picture
viewBox = color white (lineLoop [(-425,320), (500,320),(500,-320),(-425,-320), (-425,320)])

viewScore :: GameState -> Picture
viewScore gstate = pictures [scale 0.2 0.2 (translate (-2200) 2200 (color white (text "Score"))), 
                             scale 0.2 0.2 (translate (-1800) 2200 (color white (text (show (ceiling (currentScore gstate))))))]

viewPlayer :: GameState -> Picture
viewPlayer gstate = case player gstate of
  DeadPlayer -> Blank
  Player (x,y) r (_x,_y) h -> translate x y (color green (circleSolid r))

viewBullets :: GameState -> Picture
viewBullets gstate = pictures [ translate x y (color red (circleSolid r)) | Bullet (x,y) r (_x,_y) <- bullets gstate]

viewEnemy :: GameState -> Picture
viewEnemy gstate = pictures [ translate x y (color orange (circleSolid r)) | Enemy (x,y) r (_x,_y) h <- enemies gstate, x<485]

viewStar :: GameState -> Picture
viewStar gstate = pictures  [translate x y (color white (circleSolid r)) | Star (x,y) r (_x,_y)  <- background gstate, x<500]
    

    
