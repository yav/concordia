module View(View, stateView) where

import GHC.Generics(Generic)
import Data.Maybe(listToMaybe, fromMaybe)
import Data.Text(Text)
import Data.List(sortOn)
import Data.Ord(Down(..))
import Data.Map qualified as Map
import Data.Aeson(ToJSON)
import Optics
import KOI.Basics
import KOI.Bag
import Static
import State
import Types
import Log
import Score
import Version qualified as V

data View = View
  { hand :: [CardView]
  , playerInfo :: [PlayerView]
  , boardInfo :: BoardView
  , logMessages :: [[LogWord]]
  , finished :: [(PlayerId,Int)]
  , version :: Text
  } deriving (Generic,ToJSON)

type CardView = CardG (God,Int)


data PlayerView = PlayerView
  { player :: PlayerId
  , discardTop :: Maybe CardView
  , discard :: [CardView]
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
  { card :: CardView
  , cost :: [ResourceCost]
  } deriving (Generic,ToJSON)


stateView :: PlayerId -> GameState -> View
stateView pid s = View
  { hand = maybe [] (map (ourVal False)) (s ^? players % ix pid % playerHand)
  , playerInfo = pvs
  , boardInfo = boardView (s ^. board) (ourVal True)
  , logMessages = reverse (s ^. gameLog)
  , finished = case s ^. gameStatus of
                 Finished {} -> sortOn (Down . snd) (map totalScore pvs)
                 _ -> []
  , version = V.version
  }
  where
  (before,after) = break (== pid) (s ^. playerOrder)
  ourVal = fromMaybe (addValue (\_ _ -> -1)) (lookup pid godVals)
  (pvs,godVals) = 
    unzip [ playerView pid p s (s ^. playerState p) | p <- after ++ before ]
  scoreCard c = sum (map snd (cardColor c))
  totalScore pv = (player pv, sum (map scoreCard (discard pv)) +
                                   if triggeredEndGame pv then 7 else 0)


addValue :: (Bool -> God -> Int) -> Bool -> Card -> CardView
addValue val inMarket c = c { cardColor = map doAdd (cardColor c) }
  where
  doAdd g = (g, val inMarket g)
  
playerView ::
  PlayerId -> PlayerId -> GameState -> PlayerState -> 
  (PlayerView, (PlayerId, Bool -> Card -> CardView))
playerView viewBy pid gs s = (vi, (pid, godVal))
  where
  godVal = addValue (godValue pid Nothing gs)
  noVal = addValue (\_ _ -> -1)
  openInfo = case gs ^. gameStatus of
               Finished {} -> True
               _ -> False
  visible = viewBy == pid || openInfo
  thisVal = if visible then godVal False else noVal False
  vi = PlayerView
    { player = pid
    , discardTop = thisVal <$> listToMaybe (s ^. playerDiscard)
    , discard = [ thisVal c | visible, c <- s ^. playerDiscard ]
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
        Finished p -> p == pid
        _ -> False
    }

boardView :: BoardState -> (Card -> CardView) -> BoardView
boardView s addVal = BoardView
  { name = s ^. mapLayout % mapName
  , cities = cityView s 
  , paths = pathView s
  , regions = regionView s
  , market = let m = map addVal (s ^. marketDeck)
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

