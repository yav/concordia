module Setup where

import Data.Map(Map)
import Data.Map qualified as Map
import KOI.Basics
import KOI.Bag
import KOI.RNGM
import Types
import Static
import State

data Config = Config
  { cfgPlayerOrder        :: [PlayerId]
  , cfgMap                :: MapLayout
  , cfgCityTiles          :: Map CityTile [Resource]
  , cfgResourceLimit      :: Int
  , cfgStartHireWorkers   :: Bag Worker
  , cfgStartBoardWorkers  :: Bag Worker
  , cfgStartResources     :: Bag Resource
  , cfgStartMoney         :: Int
  , cfgStartHouses        :: Int

  , cfgMarket             :: [[ResourceCost]]
  , cfgPlayerCards        :: [Card]
  , cfgMarketCards        :: [ [Card] ]
    -- ^ 1st element is 1 cards, 2nd is 2, etc.
  }

setupGame :: Config -> Gen GameState
setupGame cfg0 =
  do let solo = PlayerId "Solo"
     let (cfg,first,rest) =
            case cfgPlayerOrder cfg of
              []     -> (cfg0 { cfgPlayerOrder = [solo] }, solo, [])
              p : ps -> (cfg0,p,ps)
     brd <- setupBoard cfg
     pure GameState
       { _players      = Map.fromList (zip (first:rest)
                                           (map (setupPlayer cfg) [ 0 .. ]))
       , _board        = brd
       , _curPlayer    = first
       , _prevPlayers  = []
       , _nextPlayers  = rest
       , _gameStatus   = InProgress
       }





-- | Turn order, first is 0
setupPlayer :: Config -> Int -> PlayerState
setupPlayer cfg turnOrder = PlayerState
  { _playerWorkersForHire = cfgStartHireWorkers cfg
  , _playerHousesToBuild  = cfgStartHouses cfg
  , _playerResources      = cfgStartResources cfg
  , _playerMoney          = cfgStartMoney cfg + turnOrder
  , _playerDoubleBonus    = length (cfgPlayerOrder cfg) == turnOrder + 1
  , _playerHand           = cfgPlayerCards cfg
  , _playerDiscard        = []
  , _resourceLimit        = cfgResourceLimit cfg
  }

setupBoard :: Config -> Gen BoardState
setupBoard cfg =
  do tiles <- traverse shuffle (cfgCityTiles cfg)
     let layout = cfgMap cfg
     let ps = cfgPlayerOrder cfg
     let decks = take (length ps) (cfgMarketCards cfg)
     shuffled <- mapM shuffle decks
     pure BoardState
       { mapLayout        = layout
       , _mapHouses       = mempty
       , _mapCityWorkers  = Map.singleton
                              (mapStartCity layout)
                              (foldr addStartWorker bagEmpty ps)
       , _mapPathWorkers  = mempty
       , _mapProduces =
         snd (Map.foldlWithKey' pickTile (tiles,mempty) (mapCities layout))
       , _mapPrefected    = []
       , marketLayout     = cfgMarket cfg
       , _marketDeck      = concat shuffled
       }

  where
  pickTile (tiles,produce) cityId city =
    let tileType = cityTile city in
    case Map.lookup tileType tiles of
      Just (tile : more) -> ( Map.insert tileType more tiles
                            , Map.insert cityId tile produce
                            )
      -- Shouldn't happen, but of we run out of tiles we just make bricks.
      _ -> (tiles, Map.insert cityId Brick produce)

  addStartWorker pid bs =
    bagUnion (bagMap (pid :->) (cfgStartBoardWorkers cfg)) bs


