module View(View, stateView) where

import GHC.Generics(Generic)
import Data.Maybe(listToMaybe, fromMaybe)
import Data.Text(Text)
import Data.List(sortOn)
import Data.Ord(Down(..))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Aeson(ToJSON)
import Optics
import KOI.Basics
import KOI.Bag
import Static
import State hiding (BonusChoice(..))
import State qualified
import Types
import Log
import Score
import Version qualified as V


data View = View
  { hand :: [CardView]
  , handForum :: [(ForumTile,ForumTileType)]
  , playerInfo :: [PlayerView]
  , boardInfo :: BoardView
  , logMessages :: [[LogWord]]
  , finished :: [(PlayerId,PlayerScore)]
  , version :: Text
  , attention :: Bool
  } deriving (Generic,ToJSON)

type CardView = CardG (God,Int)

data PlayerScore = PlayerScore
  { godPoints :: [(God,Int,Int)] -- card number, card value
  , concordia :: Int
  , total :: Int
  } deriving (Generic, ToJSON)

data ForumTileType = Patrician | Citizen
  deriving (Generic,ToJSON)

data PlayerView = PlayerView
  { player :: PlayerId
  , discardTop :: Maybe CardView
  , discard :: [CardView]
  , forumTiles :: [(ForumTile,ForumTileType)]
  , resources :: [ResourceSpot]
  , houses :: Int
  , money :: Int
  , handSize :: Int
  , isCurrent :: Bool
  , isDouble :: Bool
  , triggeredEndGame :: Bool
  , godScore :: [(God,Int)]
  } deriving (Generic,ToJSON)

data ResourceSpot = Available | HasWorker Worker | HasResource Resource
  deriving (Generic,ToJSON)

data BoardView = BoardView
  { name :: Text
  , cities :: [CityView]
  , paths  :: [(PathId,Maybe (WithPlayer Worker))]
  , regions :: [ (RegionId, RegionState) ]
  , market :: (Int, [MarketSpot])
  , forum :: [(Int,ForumTile)]
  } deriving (Generic,ToJSON)

data CityView = CityView
  { city :: CityId
  , produces      :: Maybe Resource
  , playerHouses  :: [PlayerId]
  , workers       :: [WithPlayer Worker]
  } deriving (Generic,ToJSON)

data RegionState = NoBonus | Variable | Goods Resource | Money Int
  deriving (Generic,ToJSON)

data MarketSpot = MarketSpot
  { card :: CardView
  , cost :: [ResourceCost]
  } deriving (Generic,ToJSON)


stateView :: PlayerId -> GameState -> View
stateView pid s = View
  { hand = 
    if settingUpForum
      then []
      else maybe [] (map (ourVal False)) (s ^? players % ix pid % playerHand)
  , handForum = 
    let fs = maybe [] Set.toList (s ^? players % ix pid % playerForumTiles)
    in forumView (if settingUpForum then fs else filter (not . isPatrician) fs)
  , playerInfo = pvs
  , boardInfo = boardView (s ^. board) (ourVal True)
  , logMessages = reverse (s ^. gameLog)
  , finished = case s ^. gameStatus of
                 Finished {} -> sortOn (Down . total . snd)
                                       (map totalScore pvs)
                 _ -> []
  , version = V.version
  , attention = case s ^. curQuestion of
                  Just p -> p == pid && Just pid /= (s ^. lastQuestion)
                  Nothing -> False
  }
  where
  settingUpForum = s ^. forumSetup
  (before,after) = break (== pid) (s ^. playerOrder)
  ourVal = fromMaybe (addValue (\_ _ -> -1)) (lookup pid godVals)

  (pvs,godVals) = 
    unzip [ playerView pid p s ps
          | p <- after ++ before
          , let ps1 = s ^. playerState p
          , let ps = if settingUpForum then over playerForumTiles (const mempty) ps1 else ps1
          ]


totalScore :: PlayerView -> (PlayerId, PlayerScore)
totalScore pv =
  (player pv, PlayerScore
  { godPoints = godVs
  , concordia = conc
  , total = conc + sum [ n * v | (_,n,v) <- godVs ]
  })
  where
  conc = if triggeredEndGame pv then 7 else 0
  godVs = [ (g, getCount g, v) | (g,v) <- godScore pv ]
  getCount g = Map.findWithDefault 0 g godCount
  godCount = Map.fromListWith (+) (concatMap doCard (discard pv))
  doCard c = map doGod (cardColor c)
  doGod (c,_) = (c,1)

  


addValue :: (Bool -> God -> Int) -> Bool -> Card -> CardView
addValue val inMarket c = c { cardColor = map doAdd (cardColor c) }
  where
  doAdd g = (g, val inMarket g)
  
playerView ::
  PlayerId -> PlayerId -> GameState -> PlayerState -> 
  (PlayerView, (PlayerId, Bool -> Card -> CardView))
playerView viewBy pid gs s = (vi, (pid, godVal))
  where
  scoreFun = godValue pid Nothing gs
  godVal = addValue scoreFun
  noVal = addValue (\_ _ -> -1)
  openInfo = case gs ^. gameStatus of
               Finished {} -> True
               _ -> False
  visible = viewBy == pid || openInfo
  thisVal = if visible then godVal False else noVal False
  allGods = [ Vesta, Jupiter, Saturnus, Venus, Mercurius, Mars ] ++
            [ Minerva g | g <- normalResources ]
  vi = PlayerView
    { player = pid
    , discardTop = thisVal <$> listToMaybe (s ^. playerDiscard)
    , discard = [ thisVal c | visible, c <- s ^. playerDiscard ]
    , forumTiles = forumView (Set.toList (s ^. playerForumTiles))
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
    , godScore = [ (g, scoreFun False g) | g <- allGods ]
    }

boardView :: BoardState -> (Card -> CardView) -> BoardView
boardView s addVal = BoardView
  { name = s ^. mapLayout % mapName
  , cities = cityView s 
  , paths = pathView s
  , regions = regionView s
  , market = let m = map addVal (s ^. marketDeck)
             in (length m, zipWith MarketSpot m (s ^. marketLayout))
  , forum = take 4 (zip [ 4, 6 .. ] (s ^. forumMarket))
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
                    State.NoBonus -> NoBonus
                    State.ResourceBonus r -> Goods r
                    State.VariableBonus -> Variable

forumView :: [ForumTile] -> [(ForumTile,ForumTileType)]
forumView xs = [ (x, if isPatrician x then Patrician else Citizen) | x <- xs ]