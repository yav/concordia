module KOI (module I, Interact, Concordia(..), Update(..)
  , askInputsMaybe_
  ) where

import Data.Text(Text)
import Optics ( (^.) )
import KOI.Basics(PlayerId(..),WithPlayer(..))
import KOI.Interact hiding (Interact,askInputsMaybe_)
import KOI.Interact qualified as I
import State ( gameStatus, GameState, GameStatus(Finished) )
import Question ( Question )
import View ( stateView, View )

data Concordia = Concordia
type Interact = I.Interact Concordia

askInputsMaybe_ ::
  PlayerId -> Text -> [(Question,Text,Interact ())] -> Interact ()
askInputsMaybe_ pid@(PlayerId name) txt opts =
  I.askInputsMaybe_ (name <> ": " <> txt)
    [ (pid :-> q,lab,act) | (q,lab,act) <- opts ] 

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


