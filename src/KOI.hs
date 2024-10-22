module KOI (module I, Interact, Concordia(..), Update(..)
  , askInputsMaybe, askInputsMaybe_, askInputs, sync, stop
  ) where

import Data.Text(Text)
import KOI.Basics(PlayerId(..),WithPlayer(..))
import KOI.Interact hiding (Interact,askInputsMaybe_,askInputs,askInputsMaybe,choose,chooseMaybe)
import KOI.Interact qualified as I
import State ( GameState, lastQuestion, curQuestion )
import Question ( Question )
import View ( stateView, View )

data Concordia = Concordia
type Interact = I.Interact Concordia

sync :: Interact ()
sync =
  do s <- getState
     update (SetState s)
     save

doQ :: PlayerId -> Interact ()
doQ pid =
  do q <- the curQuestion
     setThe lastQuestion q
     setThe curQuestion (Just pid)
     sync

askInputsMaybe_ ::
  PlayerId -> Text -> [(Question,Text,Interact ())] -> Interact ()
askInputsMaybe_ pid@(PlayerId name) txt opts =
  doQ pid >>
  I.askInputsMaybe_ (name <> ": " <> txt)
    [ (pid :-> q,lab,act) | (q,lab,act) <- opts ] 

askInputsMaybe ::
  PlayerId -> Text -> [(Question,Text,Interact a)] -> Interact (Maybe a)
askInputsMaybe pid@(PlayerId name) txt opts =
  doQ pid >>
  I.askInputsMaybe (name <> ": " <> txt)
    [ (pid :-> q,lab,act) | (q,lab,act) <- opts ] 


askInputs ::
  PlayerId -> Text -> [(Question,Text,Interact a)] -> Interact a
askInputs pid@(PlayerId name) txt opts =
  doQ pid >>
  I.askInputs (name <> ": " <> txt)
    [ (pid :-> q,lab,act) | (q,lab,act) <- opts ] 

stop :: Interact a
stop = sync >> I.askInputs "Game Over" []

instance Component Concordia where
  type AppState Concordia = GameState
  type AppStateView Concordia = View
  type AppUpdate Concordia = Update
  type AppUpdateView Concordia = View
  type AppInput Concordia = Question

  doUpdate _ (SetState s) _ = s

  finalState _ _ = False

  playerView _ = stateView

  playerUpdateView c pid (SetState s) = playerView c pid s

data Update = SetState GameState


