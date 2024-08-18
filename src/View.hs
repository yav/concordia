module View where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON)
import Optics
import KOI.Basics
import State
import Types

data View = View
  { hand :: [Card]
  , market :: [Card]
  } deriving (Generic,ToJSON)


stateView :: PlayerId -> GameState -> View
stateView _ s = View
  { hand = []
  , market = zipWith const (s ^. board % marketDeck) (s ^. board % marketLayout)
  }
