-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Graphics.Gloss as Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitSuccess, exitWith)
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | gamePhase gstate == IsStarted =
    loadScore gstate
  | gamePhase gstate == IsPlaying =
    playingStep gstate secs
  | gamePhase gstate == IsPaused =
    pauseStep gstate
  | gamePhase gstate == IsSaving =
    return exitSuccess (scoreSave gstate)
  | otherwise =
    -- Just update the elapsed time
    return gstate

loadScore :: GameState -> IO GameState
loadScore gstate = return $ gstate {player = Player (-380, 0) 20 (0, 0) 0, gamePhase = IsPlaying} --highScores = generateHighScores}

playingStep :: GameState -> Float -> IO GameState
playingStep gstate secs =
  do
    randomNumbery <- randomRIO (-300, 300) :: IO Float
    randomNumberx <- randomRIO (15, 300) :: IO Float
    randomNummerEnemy <-randomRIO (-2,2) :: IO Float
    let newNumbery = randomNumbery
        newNumberx = randomNumberx
        newNumberEnemy = randomNummerEnemy
    return $
      gstate
        { player = updatePositionPlayer (player gstate) (enemies1 gstate ++ enemies2 gstate),
          bullets = updatePositionBullet (bullets gstate) (enemies1 gstate),
          enemies1 = updatePositionEnemies1 (bullets gstate) (spawnEnemies1 gstate secs (enemies1 gstate) newNumberEnemy),
          enemies2 = updatePositionEnemies2 (bullets gstate) (spawnEnemies2 gstate secs (enemies2 gstate)) (player gstate),
          elapsedTime = elapsedTime gstate + secs,
          currentScore = currentScore gstate + elapsedTime gstate,
          randomNumberY = newNumbery,
          randomNumberX = newNumberx,
          randomNummerEnemy = newNumberEnemy,
          background = updateBackground (spawnStar gstate (background gstate))
        }

pauseStep :: GameState -> IO GameState
pauseStep gstate = return $ gstate {gamePhase = IsPaused}

scoreSave :: GameState -> IO ()
scoreSave gstate = do
  load <- readFile "highscores.txt"
  writeFile "highscores.txt" (show (currentScore gstate) ++ "\n" ++ load)

--generateHighScores :: IO String
--generateHighScores = do
--text <- readFile "highscores.txt"
--return read text :: IO String

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char c) _ _ _) gstate
  | c == 'p' && boolPause gstate == False && gamePhase gstate == IsPlaying = gstate {gamePhase = IsPaused}
  | c == 'p' && boolPause gstate == False && gamePhase gstate == IsPaused = gstate {boolPause = True}
  | c == 'p' && boolPause gstate == True && gamePhase gstate == IsPaused = gstate {gamePhase = IsPlaying}
  | c == 'p' && boolPause gstate == True && gamePhase gstate == IsPlaying = gstate {boolPause = False}
  | (c == 'w' || c == 's') && boolPlayer gstate == False && gamePhase gstate == IsPlaying = gstate {player = movePlayer c (player gstate) gstate (boolPlayer gstate), boolPlayer = True}
  | (c == 'w' || c == 's') && boolPlayer gstate == True && gamePhase gstate == IsPlaying = gstate {player = movePlayer c (player gstate) gstate (boolPlayer gstate), boolPlayer = False}
  | c == 'f'  && boolBullet gstate == False && gamePhase gstate == IsPlaying = gstate {bullets = startPositionBullet (player gstate) : bullets gstate, boolBullet = True}
  | c == 'f'  && boolBullet gstate == True && gamePhase gstate == IsPlaying = gstate {boolBullet = False}
  | c == 'x' = gstate {gamePhase = IsSaving}
  | otherwise -- If the user presses a character key, show that one
    =
    gstate {infoToShow = ShowAChar c}
inputKey _ gstate = gstate -- Otherwise keep the same

movePlayer :: Char -> Player -> GameState -> Bool -> Player
movePlayer c (Player (x, y) r (_x, _y) h) gstate a
  | c == 'w' && _y <= 3 && a == False = Player (x, y) r (_x, 5) h
  | c == 's' && _y >= (-3) && a == False = Player (x, y) r (_x, -5) h
  | a == True = Player (x,y) r (_x,0) h
  | otherwise = Player (x, y) r (_x, 0) h
movePlayer c DeadPlayer gstate a = DeadPlayer

startPositionBullet :: Player -> Bullet
startPositionBullet DeadPlayer = NoBullet
startPositionBullet (Player (x, y) r (_x, _y) h) = Bullet (x + r, y) 7 (3, 0)

spawnStar :: GameState -> [Star] -> [Star]
spawnStar gstate xs
  | length xs < 500 = Star (500 + randomNumberX gstate, randomNumberY gstate) 1 (-2.5, 0) : xs
  | otherwise = xs

spawnEnemies1 :: GameState -> Float -> [Enemy] -> Float -> [Enemy]
spawnEnemies1 gstate secs xs z
  | (round (elapsedTime gstate + secs) `mod` (waveNumber gstate)) == 0 && length xs < (round ((elapsedTime gstate + secs) / 10) + 3) = Enemy (500 + randomNumberX gstate, randomNumberY gstate) 15 (-(1.005** elapsedTime gstate), z) 1 : xs
  | otherwise = xs

spawnEnemies2 :: GameState -> Float -> [Enemy] -> [Enemy]
spawnEnemies2 gstate secs xs
  | (round (elapsedTime gstate + secs) `mod` (waveNumber gstate)) == 0 && length xs < (round ((elapsedTime gstate + secs) / 15 )) = Enemy (500 + randomNumberX gstate, randomNumberY gstate) 15 (-(1.001** elapsedTime gstate), 3) 1 : xs
  | otherwise = xs

updatePositionPlayer :: Player -> [Enemy] -> Player
updatePositionPlayer (Player (x, y) r (_x, _y) h) xs
  | y <= 300 && y >= (-300) && (collisionPlayerEnemy (Player (x,y) r (_x,_y) h) xs == False) = Player (x, y + _y) r (_x, _y) h
  | y > 300 = Player (x, 300) r (_x, 0) h
  | y < -300 = Player (x, -300) r (_x, 0) h
updatePositionPlayer (Player (_, _) _ (_, _) _) _ = DeadPlayer
updatePositionPlayer DeadPlayer _  = DeadPlayer

updatePositionBullet :: [Bullet] -> [Enemy] -> [Bullet]
updatePositionBullet [] _ = []
updatePositionBullet [NoBullet] _ = []
updatePositionBullet ((Bullet (x, y) r (_x, _y)) : xs) ys
  | x < 495 &&  (collisionBulletEnemies (Bullet (x,y) r (_x,_y)) ys == False) = Bullet (x + _x, y) r (_x, _y) : updatePositionBullet xs ys
  | otherwise = updatePositionBullet xs ys

updatePositionEnemies1 :: [Bullet] -> [Enemy] -> [Enemy]
updatePositionEnemies1 _ []  = []
updatePositionEnemies1 _ [DeadEnemy] = []
updatePositionEnemies1 ys ((Enemy (x, y) r (_x, _y) h) : xs) 
  | x > -400  && y < 300 && y> -300 &&  (collisionEnemyBullets ys (Enemy (x, y) r (_x, _y) h) == False) = Enemy (x + _x, y +_y) r (_x, _y) h : updatePositionEnemies1 ys xs 
  | x > -400  && y >= 300 && (collisionEnemyBullets ys (Enemy (x, y) r (_x, _y) h) == False) = Enemy (x + _x, 298 +_y) r (_x, - _y) h : updatePositionEnemies1 ys xs 
  | x > -400  && y <= -300 && (collisionEnemyBullets ys (Enemy (x, y) r (_x, _y) h) == False) = Enemy (x + _x, -298+_y) r (_x, - _y) h : updatePositionEnemies1 ys xs 
  | otherwise = updatePositionEnemies1 ys xs 

updatePositionEnemies2 :: [Bullet] -> [Enemy] -> Player -> [Enemy]
updatePositionEnemies2 _ [] _ = []
updatePositionEnemies2 _ [DeadEnemy] _ = []
updatePositionEnemies2 ys ((Enemy (x1, y1) r1 (_x1, _y1) h1) : xs) (Player (x2,y2) r2 (_x2,_y2) h2)
  | x1 > -400  && y1 < 300 && y1 <= y2 - 5*r2 && (collisionEnemyBullets ys (Enemy (x1, y1) r1 (_x1, _y1) h1) == False) = Enemy (x1 + _x1, y1+_y1) r1 (_x1, 3) h1 : updatePositionEnemies2 ys xs (Player (x2,y2) r2 (_x2,_y2) h2)
  | x1 > -400  && y1 >= 300 && (collisionEnemyBullets ys (Enemy (x1, y1) r1 (_x1, _y1) h1) == False) = Enemy (x1 + _x1, 297 +_y1) r1 (_x1, - 3) h1 : updatePositionEnemies2 ys xs (Player (x2,y2) r2 (_x2,_y2) h2)
  | x1 > -400  && y1 < 300 && y1 > -300 && y1 > y2 - 5*r2 && y1 < y2 + 5*r2 && (collisionEnemyBullets ys (Enemy (x1, y1) r1 (_x1, _y1) h1) == False) = Enemy (x1 + _x1, y1+_y1) r1 (_x1, _y1) h1 : updatePositionEnemies2 ys xs (Player (x2,y2) r2 (_x2,_y2) h2)
  | x1 > -400  && y1 > -300 && y1 >= y2 + 5*r2 && (collisionEnemyBullets ys (Enemy (x1, y1) r1 (_x1, _y1) h1) == False) = Enemy (x1 + _x1, y1+_y1) r1 (_x1, -3) h1 : updatePositionEnemies2 ys xs (Player (x2,y2) r2 (_x2,_y2) h2)
  | x1 > -400  && y1 <= -300 && (collisionEnemyBullets ys (Enemy (x1, y1) r1 (_x1, _y1) h1) == False) = Enemy (x1 + _x1, -297+_y1) r1 (_x1, 3) h1 : updatePositionEnemies2 ys xs (Player (x2,y2) r2 (_x2,_y2) h2)
  | otherwise = updatePositionEnemies2 ys xs (Player (x2,y2) r2 (_x2,_y2) h2)

updateBackground :: [Star] -> [Star]
updateBackground [] = []
updateBackground [NoStar] = []
updateBackground ((Star (x, y) r (_x, _y)) : xs)
  | x > -425 = Star (x + _x, y) r (_x, _y) : updateBackground xs
  | otherwise = updateBackground xs

collisionEnemyBullets :: [Bullet] -> Enemy -> Bool
collisionEnemyBullets [] _ = False
collisionEnemyBullets ((Bullet (x1,y1) r1 (_x1,_y1)):xs) (Enemy (x2,y2) r2 (_x2,_y2) h)  
  | sqrt ((x2-x1)^2 + (y2-y1)^2) <(r1+r2) = True
  | otherwise = collisionEnemyBullets xs (Enemy (x2,y2) r2 (_x2,_y2) h)

collisionBulletEnemies :: Bullet -> [Enemy] -> Bool
collisionBulletEnemies _ [] = False
collisionBulletEnemies (Bullet (x1,y1) r1 (_x1,_y1)) ((Enemy (x2,y2) r2 (_x2,_y2) h) : xs)
  | sqrt ((x2-x1)^2 + (y2-y1)^2) <(r1+r2) = True
  | otherwise = collisionBulletEnemies (Bullet (x1,y1) r1 (_x1,_y1)) xs

collisionPlayerEnemy :: Player -> [Enemy] -> Bool
collisionPlayerEnemy _ [] = False
collisionPlayerEnemy (Player (x1,y1) r1 (_x1,_y1) h1) ((Enemy (x2,y2) r2 (_x2,_y2) h) : xs)
  | sqrt ((x2-x1)^2 + (y2-y1)^2) <(r1+r2) || x2 <= (-400)  = True
  | otherwise = collisionPlayerEnemy (Player (x1,y1) r1 (_x1,_y1) h1) xs


                        

