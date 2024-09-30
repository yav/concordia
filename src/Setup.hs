module Setup where

import Data.Maybe(fromMaybe)
import Data.Map(Map)
import Data.Map qualified as Map
import Optics
import KOI.Basics
import KOI.Bag
import KOI.RNGM
import Types
import Constants
import Static
import State
import Maps

data Config = Config
  { cfgPlayers           :: [PlayerId]
  , cfgMap                :: MapLayout
  , cfgMapName            :: Maps
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
     cfg <-
        case cfgPlayers cfg0 of
          []     -> pure cfg0 { cfgPlayers = [solo] }
          ps ->
            do qs <- shuffle ps
               pure cfg0 { cfgPlayers = qs }
     let (first,rest) = case cfgPlayers cfg of
                          a : as -> (a,as)
                          _ -> error "setupGame []"
     brd <- setupBoard cfg
     pure GameState
       { _players      = Map.fromList (zip (first:rest)
                                           (map (setupPlayer cfg) [ 0 .. ]))
       , _board        = brd
       , _curPlayer    = first
       , _playerOrder  = first : rest
       , _gameStatus   = InProgress
       , _playerDoubleBonus = last (first : rest)
       , _gameLog      = []
       }





-- | Turn order, first is 0
setupPlayer :: Config -> Int -> PlayerState
setupPlayer cfg turnOrder = PlayerState
  { _playerWorkersForHire = cfgStartHireWorkers cfg
  , _playerHousesToBuild  = cfgStartHouses cfg
  , _playerResources      = cfgStartResources cfg
  , _playerMoney          = cfgStartMoney cfg + turnOrder
  , _playerHand           = cfgPlayerCards cfg
  , _playerDiscard        = []
  , _playerResourceLimit  = cfgResourceLimit cfg
  , _playerPowers         = []
  }

setupBoard :: Config -> Gen BoardState
setupBoard cfg =
  do tiles <- traverse shuffle (cfgCityTiles cfg)
     let layout = cfgMap cfg
     let ps = cfgPlayers cfg
     let decks = take (length ps) (cfgMarketCards cfg)
     shuffled <- mapM shuffle decks
     let cityProd = snd (Map.foldlWithKey' pickTile (tiles,mempty)
                        (layout ^. mapCities))
     let cityNum =
            let add acc c = Map.insertWith (+) (c ^. cityRegion) (1::Int) acc
                mp = Map.foldl' add mempty (layout ^. mapCities)
             in \rid -> Map.findWithDefault 0 rid mp

     let regBonus mp cid city =
          fromMaybe mp
          do resource <- Map.lookup cid cityProd
             cost     <- Map.lookup resource resourceCost
             let bigger (res1,cost1) (res2,cost2) =
                   if cost1 > cost2 then (res1,cost1) else (res2,cost2)
             pure (Map.insertWith bigger (city ^. cityRegion) (resource,cost) mp)

     pure BoardState
       { _mapLayout       = layout
       , _mapHouses       = mempty
       , _mapCityWorkers  = Map.singleton
                              (layout ^. mapStartCity)
                              (foldr addStartWorker bagEmpty ps)
       , _mapPathWorkers  = mempty
       , _mapRegionBonus  =
          let mk rid (r,_) =
               if cfgMapName cfg == Crete && cityNum rid == 1
                then RegionBonus { _rbResource = VariableBonus, _rbMoney = 0 }
                else
                RegionBonus { _rbResource = ResourceBonus r
                            , _rbMoney = Map.findWithDefault 0 r
                                                        resourcePrefectMoney }
          in Map.mapWithKey mk (Map.foldlWithKey' regBonus mempty (layout ^. mapCities))
       , _mapProduces     = cityProd
       , _mapPrefected    = []
       , _marketLayout    = cfgMarket cfg
       , _marketDeck      = concat shuffled
       , _mapExtraMoneyBonus =
          if cfgMapName cfg == Crete then 2 else 0
       }

  where
  pickTile (tiles,produce) cityId city =
    let tileType = city ^. cityTile in
    case Map.lookup tileType tiles of
      Just (tile : more) -> ( Map.insert tileType more tiles
                            , Map.insert cityId tile produce
                            )
      _ -> (tiles, produce)

  addStartWorker pid = bagUnion (bagMap (pid :->) (cfgStartBoardWorkers cfg))

