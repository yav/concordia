module KOI (module I, Interact, Concordia(..)) where

import Optics
import Data.Aeson(ToJSON,FromJSON)
import GHC.Generics(Generic)
import KOI.Interact hiding (Interact)
import KOI.Interact qualified as I
import State

data Concordia = Concordia
type Interact = I.Interact Concordia

instance Component Concordia where
  type AppState Concordia = GameState
  type AppStateView Concordia = StateView
  type AppUpdate Concordia = Update
  type AppUpdateView Concordia = StateView
  type AppInput Concordia = Input

  doUpdate _ (SetState s) _ = s

  finalState _ s =
    case s ^. gameStatus of
      Finished {} -> True
      _           -> False

  playerView _ _pid _s = StateView

  playerUpdateView c pid (SetState s) = playerView c pid s

data Update = SetState GameState

-- XXX
data StateView = StateView
  deriving (Generic,ToJSON)

-- XXX
data Input = Input
  deriving (Show,Read,Eq,Ord,Generic,FromJSON,ToJSON)
