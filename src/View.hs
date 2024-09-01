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
import Log

data View = View
  { hand :: [Card]
  , playerInfo :: [PlayerView]
  , boardInfo :: BoardView
  , logMessages :: [[LogWord]]
  } deriving (Generic,ToJSON)

data PlayerView = PlayerView
  { player :: PlayerId
  , discardTop :: Maybe Card
  , discard :: [Card]
  , resources :: [ResourceSpot]
  , houses :: Int
  , money :: Int
  , handSize :: Int
  , isCurrent :: Bool
  , isDouble :: Bool
  , triggeredEndGame :: Bool
  } deriving (Generic,ToJSON)

data ResourceSpot = Available | HasWorker Worker | HasResource Resource
  deriving (Generic,ToJSON)

data BoardView = BoardView
  { name :: Text
  , cities :: [CityView]
  , paths  :: [(PathId,Maybe (WithPlayer Worker))]
  , regions :: [ (RegionId, RegionState) ]
  , market :: (Int, [MarketSpot])
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
      [ playerView pid p s (s ^. playerState p) | p <- after ++ before ]
  , boardInfo = boardView (s ^. board)
  , logMessages = reverse (s ^. gameLog)
  }
  where
  (before,after) = break (== pid) (s ^. playerOrder)


playerView :: PlayerId -> PlayerId -> GameState -> PlayerState -> PlayerView
playerView viewBy pid gs s = PlayerView
  { player = pid
  , discardTop = listToMaybe (s ^. playerDiscard)
  , discard = [ c | pid == viewBy, c <- s ^. playerDiscard ]
  , houses = s ^. playerHousesToBuild
  , resources =
    take (s ^. playerResourceLimit)
       (map HasWorker (bagToList (s ^. playerWorkersForHire)) ++
        map HasResource (bagToList (s ^. playerResources)) ++
        repeat Available)
  , money = s ^. playerMoney
  , handSize = length (s ^. playerHand)
  , isCurrent = gs ^. curPlayer == pid
  , isDouble = gs ^. playerDoubleBonus == pid
  , triggeredEndGame =
    case gs ^. gameStatus of
      EndTriggeredBy p -> p == pid
      _ -> False
  }

boardView :: BoardState -> BoardView
boardView s = BoardView
  { name = s ^. mapLayout % mapName
  , cities = cityView s 
  , paths = pathView s
  , regions = regionView s
  , market = let m = s ^. marketDeck
             in (length m, zipWith MarketSpot m (s ^. marketLayout))
  }

pathView :: BoardState -> [ (PathId, Maybe (WithPlayer Worker)) ]
pathView s =
  [ (eid, Map.lookup eid (s ^. mapPathWorkers))
  | eid <- Map.keys (s ^. mapLayout % mapPaths)
  ]

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

