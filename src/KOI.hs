module KOI (module I, Interact, Concordia(..), Update(..)) where

import Optics
import KOI.Interact hiding (Interact)
import KOI.Interact qualified as I
import State
import Question
import View

data Concordia = Concordia
type Interact = I.Interact Concordia

instance Component Concordia where
  type AppState Concordia = GameState
  type AppStateView Concordia = View
  type AppUpdate Concordia = Update
  type AppUpdateView Concordia = View
  type AppInput Concordia = Question

  doUpdate _ (SetState s) _ = s

  finalState _ s =
    case s ^. gameStatus of
      Finished {} -> True
      _           -> False

  playerView _ = stateView

  playerUpdateView c pid (SetState s) = playerView c pid s

data Update = SetState GameState


