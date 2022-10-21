-- | This module contains the data types
--   which represent the state of the game
module Model where

type Health = Int
type Score = Float
type Time = Float

data InfoToShow = ShowNothing
                | ShowAChar   Char

data GameStates = IsPaused | IsPlaying | IsFinished | IsStarted | IsSaving | ShouldStart deriving (Eq, Show)


data GameState = GameState {
                    infoToShow  :: InfoToShow
                   , elapsedTime :: Float
                   , currentScore :: Score
                   , gamePhase :: GameStates
                  }                

initialState :: GameState
initialState = GameState ShowNothing 0 0 IsPlaying