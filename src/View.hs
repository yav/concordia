module View(View, stateView) where

import GHC.Generics(Generic)
import Data.Maybe(listToMaybe, fromMaybe)
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

data CityView = CityView
  { city :: CityId
  , produces :: Maybe Resource
  , houses :: [PlayerId]
  , members :: [WithPlayerId Worker]
  } deriving (Generic,ToJSON)

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
  { hand = fromMaybe [] (s ^? players % ix pid % playerHand)
  , playerInfo =
      [ playerView p (s ^. playerState p) | p <- s ^. after ++ bedore ]
  , boardInfo = boardView (s ^. board)
  }
  where
  (before,after) = break (== pid) (s ^. playerOrder)


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
boardView s = BoardView
  { name = s ^. mapLayout % mapName
  , cities = cityView s
  , paths = [] -- XXX
  , regions = mempty -- XXX
  , market = zipWith MarketSpot (s ^. marketDeck) (s ^. marketLayout)
  }

cityView :: BoardState -> [CityView]
cityView s =
  [ CityView { city = cid,
               produces = Map.lookup cid (s ^. mapProduces)
               members = Map.fromListWith (++)
                            (Map.findWithDefault [] cid getMembers)
             }
  | cid <- Map.keys (s ^. mapLayout % mapCities)
  ]
  where
  workers ws = [ (p, [CityWorker w]) | p :-> w <- bagToList ws ]
  getHouses ps = [ (p, [House]) | p <- ps ]
  getMembers = Map.unionWith (++) (workers <$> (s ^. mapCityWorkers))
                                  (getHouses <$> (s ^. mapHouses)) 
