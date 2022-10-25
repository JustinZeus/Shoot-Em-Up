-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss as Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | gamePhase gstate == IsPlaying 
  = -- We show a new random number
    do randomNumbery <- randomRIO (-300,300) :: IO Float
       randomNumberx <- randomRIO (15,300) :: IO Float
       let newNumbery = randomNumbery 
           newNumberx = randomNumberx
       return $ gstate {
                        player = updatePositionPlayer (player gstate),
                        bullets = updatePositionBullet (bullets gstate), 
                        enemies = updatePositionEnemies (spawnEnemies gstate secs (enemies gstate)), 
                        elapsedTime = elapsedTime gstate + secs, 
                        currentScore = currentScore gstate + elapsedTime gstate, 
                        randomNumberY = newNumbery, 
                        randomNumberX = newNumberx,
                        background = updateBackground (spawnStar gstate (background gstate))
                        }
  | gamePhase gstate == IsPaused
  =
    return $ gstate {gamePhase = IsPaused}
  | otherwise
  = -- Just update the elapsed time
    return $ gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate 
    | c == 'p' && gamePhase gstate == IsPlaying = gstate {gamePhase = IsPaused}
    | c == 'o' && gamePhase gstate == IsPaused = gstate {gamePhase = IsPlaying}
    | (c == 'w' || c == 's') && (currentState gstate) == IsPlaying = gstate {player =  movePlayer c (player gstate) gstate} 
    | c == 'f' && (currentState gstate) == IsPlaying = gstate {bullets = (startPositionBullet (player gstate)):(bullets gstate)}
    | otherwise-- If the user presses a character key, show that one
     = gstate { infoToShow = ShowAChar c }

inputKey _ gstate = gstate -- Otherwise keep the same


movePlayer :: Char -> Player -> GameState -> Player
movePlayer c (Player (x,y) r (_x,_y) h) gstate 
  | c == 'w' && currentState gstate == IsPlaying && _y<=3 = Player (x,y) r (_x,_y+3) h
  | c == 's' && currentState gstate == IsPlaying && _y>=(-3) = Player (x,y) r (_x,_y-3) h
  | otherwise = Player (x,y) r (_x,_y) h
movePlayer c DeadPlayer gstate = DeadPlayer

updatePositionPlayer :: Player -> Player
updatePositionPlayer (Player (x,y) r (_x,_y) h) | y<=300 && y>=(-300) = Player (x,y+_y) r (_x,_y) h
                                                | y>300  = Player (x,300) r (_x,0) h
                                                | y<(-300) = Player (x,(-300)) r (_x,0) h
updatePositionPlayer DeadPlayer = DeadPlayer

startPositionBullet :: Player  -> Bullet 
startPositionBullet DeadPlayer = NoBullet
startPositionBullet (Player (x,y) r (_x,_y) h) = Bullet (x+r,y) 7 (3,0)

updatePositionBullet :: [Bullet] -> [Bullet]
updatePositionBullet [] = []
updatePositionBullet [NoBullet] = []
updatePositionBullet ((Bullet (x,y) r (_x,_y)):xs) | x < 495 = ((Bullet (x+_x,y) r (_x,_y)) : updatePositionBullet xs)
                                                   | otherwise = updatePositionBullet xs

spawnEnemies :: GameState -> Float -> [Enemy] -> [Enemy]
spawnEnemies gstate secs (xs) | ((round (elapsedTime gstate + secs)) `mod` 20) == 0  && length xs < (round (((elapsedTime gstate + secs))/10) + 3) = (Enemy (500+randomNumberX gstate,randomNumberY gstate) 15 (-(1.005**(elapsedTime gstate)),0) 1):xs
                              | otherwise = xs



updatePositionEnemies :: [Enemy] -> [Enemy]
updatePositionEnemies [] = []
updatePositionEnemies [DeadEnemy] = []
updatePositionEnemies ((Enemy (x,y) r (_x,_y) h):xs) | x > -400 = (Enemy (x+_x,y) r (_x,_y) h) : (updatePositionEnemies xs)
                                                    | otherwise = updatePositionEnemies xs

spawnStar :: GameState -> [Star] -> [Star]
spawnStar gstate (xs) | length xs < 300 = (Star (500+randomNumberX gstate,randomNumberY gstate) 1 (-2.5,0)):xs
                      | otherwise = xs

updateBackground :: [Star] -> [Star]
updateBackground [] = [] 
updateBackground [NoStar] = []
updateBackground ((Star (x,y) r (_x,_y)):xs) | x > -425 = (Star (x+_x,y) r (_x,_y)) : (updateBackground xs)
                                             | otherwise = updateBackground xs

