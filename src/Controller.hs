-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  | gamePhase gstate == IsPlaying
  = -- We show a new random number
    return $ gstate { elapsedTime = elapsedTime gstate + secs, currentScore = currentScore gstate + elapsedTime gstate}
  |gamePhase gstate == IsPaused
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
    | otherwise-- If the user presses a character key, show that one
     = gstate { infoToShow = ShowAChar c }

inputKey _ gstate = gstate -- Otherwise keep the same