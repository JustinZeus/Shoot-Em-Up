-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [scorePicture, pressedKey]
              where
                scorePicture = viewScore gstate
                pressedKey = viewKey gstate

viewScore :: GameState -> Picture
viewScore gstate = pictures [scale 0.2 0.2 (translate (-2200) 2200 (color white (text "Score"))), 
                            scale 0.2 0.2 (translate (-1800) 2200 (color white (text (show (ceiling (currentScore gstate))))))]

viewKey :: GameState -> Picture
viewKey gstate = case infoToShow gstate of
  ShowNothing   -> blank
  ShowAChar   c -> color green (text [c])
