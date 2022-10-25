-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss as Gloss

type Health = Float

type Score = Float

type Time = Float

data GameStates = IsPlaying | IsPaused | IsFinished | IsStarted | IsSaving deriving (Show, Eq)

data InfoToShow
  = ShowNothing
  | ShowAChar Char

data GameState = GameState
  { infoToShow :: InfoToShow,
    elapsedTime :: Float,
    currentScore :: Score,
    gamePhase :: GameStates,
    player :: Player,
    enemies :: [Enemy],
    bullets :: [Bullet],
    highScores :: [[Float]],
    randomNumberX :: Float,
    randomNumberY :: Float,
    background :: [Star]
  }

data Player
  = DeadPlayer
  | Player
      { locationPlayer :: Gloss.Point,
        sizePlayer :: Float,
        velocityPlayer :: Gloss.Vector,
        healthPlayer :: Health
      }
  deriving (Eq, Show)

data Enemy
  = DeadEnemy
  | Enemy
      { locationEnemy :: Gloss.Point,
        sizeEnemy :: Float,
        velocityEnemy :: Gloss.Vector,
        healthEnemy :: Health
      }
  deriving (Eq, Show)

data Bullet
  = NoBullet
  | Bullet
      { locationBullet :: Gloss.Point,
        sizeBullet :: Float,
        velocityBullet :: Gloss.Vector
      }
  deriving (Eq, Show)

data Star
  = NoStar
  | Star
      { locationStar :: Gloss.Point,
        sizeStar :: Float,
        velocityStar :: Gloss.Vector
      }
  deriving (Eq, Show)

initialState :: GameState
initialState = GameState ShowNothing 0 0 IsPlaying (Player (-380, 0) 20 (0, 0) 0) [] [] [[]] 0 0 []
