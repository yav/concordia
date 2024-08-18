module View(View, stateView) where

import GHC.Generics(Generic)
import Data.Maybe(listToMaybe)
import Data.Text(Text)
import Data.Map(Map)
import Data.Map qualified as Map
import Data.Aeson(ToJSON)
import Optics
import KOI.Basics
import KOI.Bag
import Static
import State
import Types

data View = View
  { hand :: [Card]
  , playerInfo :: [PlayerView]
  , boardInfo :: BoardView
  } deriving (Generic,ToJSON)

data PlayerView = PlayerView
  { player :: PlayerId
  , discard :: Maybe Card
  , resources :: [ResourceSpot]
  , houses :: Int
  , money :: Int
  , handSize :: Int
  } deriving (Generic,ToJSON)

data ResourceSpot = Available | HasWorker Worker | HasResource Resource
  deriving (Generic,ToJSON)

data BoardView = BoardView
  { name :: Text
  , cities :: [CityView]
  , paths  :: [PathView]
  , regions :: Map RegionId RegionState
  , market :: [MarketSpot]
  } deriving (Generic,ToJSON)

data CityView = CityViw
  { city :: CityId
  , produces :: Resource
  , memebrs :: Map PlayerId [CityMember]
  } deriving (Generic,ToJSON)

data CityMember = CityWorker Worker | House
  deriving (Generic,ToJSON)

data PathView = PathView
  { path :: PathId
  , owner :: PlayerId
  , worker :: Worker
  } deriving (Generic,ToJSON)

data RegionState = Disabled | Produced Resource | Prefected Int
  deriving (Generic,ToJSON)

data MarketSpot = MarketSpot
  { card :: Card
  , cost :: [ResourceCost]
  } deriving (Generic,ToJSON)


stateView :: PlayerId -> GameState -> View
stateView pid s = View
  { hand = case s ^? players % ix pid % playerHand of
             Just cs -> cs
             Nothing -> []
  , playerInfo =
      [ playerView p (s ^. playerState p) | p <- s ^. playerOrder ]
  , boardInfo = boardView (s ^. board)
  }


playerView :: PlayerId -> PlayerState -> PlayerView
playerView pid s = PlayerView
  { player = pid
  , discard = listToMaybe (s ^. playerDiscard)
  , houses = s ^. playerHousesToBuild
  , resources =
    take (s ^. playerResourceLimit)
       (map HasWorker (bagToList (s ^. playerWorkersForHire)) ++
        map HasResource (bagToList (s ^. playerResources)) ++
        repeat Available)
  , money = s ^. playerMoney
  , handSize = length (s ^. playerHand)
  }

boardView :: BoardState -> BoardView
boardView = undefined

