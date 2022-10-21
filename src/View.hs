-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [scorePicture, pressedKey,playerPicture, bulletPicture]
              where
                scorePicture = viewScore gstate
                pressedKey = viewKey gstate
                playerPicture = viewPlayer gstate
                bulletPicture = viewBullets gstate

viewScore :: GameState -> Picture
viewScore gstate = pictures [scale 0.2 0.2 (translate (-2200) 2200 (color white (text "Score"))), 
                             scale 0.2 0.2 (translate (-1800) 2200 (color white (text (show (ceiling (currentScore gstate))))))]

viewKey :: GameState -> Picture
viewKey gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowAChar   c -> color green (text [c])


viewPlayer :: GameState -> Picture
viewPlayer gstate = case player gstate of
  DeadPlayer -> Blank
  Player (x,y) a (_x,_y) h -> translate x y (color green (circleSolid a))

viewBullets :: GameState -> Picture
viewBullets gstate = pictures [ translate x y (color red (circleSolid a)) | Bullet (x,y) a (_x,_y) <- bullets gstate]
    

    
