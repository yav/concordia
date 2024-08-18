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
stateView p s = View
  { hand = s ^. playerState p % playerHand
  , market = zipWith const (s ^. board % marketDeck) (s ^. board % marketLayout)
  }
