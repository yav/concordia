module View(View, stateView) where

import GHC.Generics(Generic)
import Data.Maybe(listToMaybe, fromMaybe)
import Data.Text(Text)
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
  , paths  :: [(PathId,WithPlayer Worker)]
  , regions :: [ (RegionId, RegionState) ]
  , market :: [MarketSpot]
  } deriving (Generic,ToJSON)

data CityView = CityView
  { city :: CityId
  , produces      :: Maybe Resource
  , playerHouses  :: [PlayerId]
  , workers       :: [WithPlayer Worker]
  } deriving (Generic,ToJSON)

data RegionState = NoBonus | Goods Resource | Money Int
  deriving (Generic,ToJSON)

data MarketSpot = MarketSpot
  { card :: Card
  , cost :: [ResourceCost]
  } deriving (Generic,ToJSON)


stateView :: PlayerId -> GameState -> View
stateView pid s = View
  { hand = fromMaybe [] (s ^? players % ix pid % playerHand)
  , playerInfo =
      [ playerView p (s ^. playerState p) | p <- after ++ before ]
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
  , paths = Map.toList (s ^. mapPathWorkers)
  , regions = regionView s
  , market = zipWith MarketSpot (s ^. marketDeck) (s ^. marketLayout)
  }

cityView :: BoardState -> [CityView]
cityView s =
  [ CityView { city = cid
             , produces = Map.lookup cid (s ^. mapProduces)
             , playerHouses = Map.findWithDefault [] cid (s ^. mapHouses)
             , workers =
                bagToList
                 (Map.findWithDefault bagEmpty cid (s ^. mapCityWorkers))
             }
  | cid <- Map.keys (s ^. mapLayout % mapCities)
  ]

regionView :: BoardState -> [ (RegionId, RegionState) ]
regionView s =
  [ (rid, rstate rid bonus) | (rid,bonus) <- Map.toList (s ^. mapRegionBonus) ]
  where
  pref = s ^. mapPrefected
  rstate rid bon
    | rid `elem` pref = Money (bon ^. rbMoney)
    | otherwise = case bon ^. rbResource of
                    Nothing -> NoBonus
                    Just r  -> Goods r

