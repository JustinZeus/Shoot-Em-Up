-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [backgroundPicture, scorePicture, playerPicture, bulletPicture, enemy1Picture, enemy2Picture, viewBox, highScoreTest, newWavePicture]
  where
    scorePicture = viewScore gstate
    playerPicture = viewPlayer gstate
    bulletPicture = viewBullets gstate
    enemy1Picture = viewEnemy1 gstate
    enemy2Picture = viewEnemy2 gstate
    backgroundPicture = viewStar gstate
    highScoreTest = viewHighScores gstate
    newWavePicture = viewNewWave gstate

viewBox :: Picture
viewBox = color white (lineLoop [(-425, 320), (500, 320), (500, -320), (-425, -320), (-425, 320)])

viewScore :: GameState -> Picture
viewScore gstate =
  pictures
    [ scale 0.2 0.2 (translate 1600 1750 (color white (text "Score"))),
      scale 0.2 0.2 (translate 2000 1750 (color white (text (show (ceiling (currentScore gstate))))))
    ]

viewKey :: GameState -> Picture
viewKey gstate = case infoToShow gstate of
  ShowNothing -> blank
  ShowAChar c -> blank --color green (text [c])

viewPlayer :: GameState -> Picture
viewPlayer gstate = case player gstate of
  DeadPlayer -> Blank
  Player (x, y) r (_x, _y) h -> translate x y (head (img gstate))

viewBullets :: GameState -> Picture
viewBullets gstate = pictures [translate x y (color red (circleSolid r)) | Bullet (x, y) r (_x, _y) <- bullets gstate]

viewEnemy1 :: GameState -> Picture
viewEnemy1 gstate = pictures [translate x y (color orange (circleSolid r)) | Enemy (x, y) r (_x, _y) h <- enemies1 gstate, x < 485]

viewEnemy2 :: GameState -> Picture
viewEnemy2 gstate = pictures [translate x y (img gstate !! 1) | Enemy (x, y) r (_x, _y) h <- enemies2 gstate, x < 485]

viewStar :: GameState -> Picture
viewStar gstate = pictures [translate x y (color white (circleSolid r)) | Star (x, y) r (_x, _y) <- background gstate, x < 500]

viewHighScores :: GameState -> Picture
viewHighScores gstate = case gamePhase gstate of
  IsPaused ->
    pictures
      [ scale 0.2 0.2 (translate (-1800) 2500 (color white (text "Highscores:"))),
        scale 0.2 0.2 (translate (-1800) 2350 (color white (text (show (head (highScores gstate)))))),
        scale 0.2 0.2 (translate (-1800) 2200 (color white (text (show (highScores gstate !! 1))))),
        scale 0.2 0.2 (translate (-1800) 2050 (color white (text (show (highScores gstate !! 2))))),
        scale 0.2 0.2 (translate (-1800) 1900 (color white (text (show (highScores gstate !! 3))))),
        scale 0.2 0.2 (translate (-1800) 1750 (color white (text (show (highScores gstate !! 4)))))
        --scale nr 1 uit lijst
        --tot nr 5
      ]
  _ -> blank

viewNewWave :: GameState -> Picture
viewNewWave gstate | ((ceiling (elapsedTime gstate)) `mod` (waveNumber gstate)) == (waveNumber gstate)-1 = pictures [scale 0.2 0.2 (translate (-50) 0 (color white (text "New Wave")))]
                    | otherwise = blank

