module State where

import Data.Map(Map)
import Data.Map qualified as Map

import Optics
import KOI.Basics
import KOI.Bag

import Types
import Static

data GameState = GameState
  { _players      :: !(Map PlayerId PlayerState)
  , _board        :: !BoardState
  , _curPlayer    :: !PlayerId
  , _prevPlayers  :: ![PlayerId]   -- ^ Most recent first
  , _nextPlayers  :: ![PlayerId]
  , _playerDoubleBonus :: !PlayerId
  , _gameStatus   :: !GameStatus
  }

data PlayerState = PlayerState
  { _playerWorkersForHire :: !(Bag Worker)
  , _playerHousesToBuild  :: !Int
  , _playerResources      :: !(Bag Resource)
  , _playerMoney          :: !Int
  , _playerHand           :: ![Card]
  , _playerDiscard        :: ![Card]   -- ^ Most recent first
  , _playerResourceLimit  :: !Int      -- ^ Including workers
  }

data BoardState = BoardState
  { mapLayout        :: !MapLayout
  , _mapHouses       :: !(Map CityId [PlayerId])
  , _mapCityWorkers  :: !(Map CityId (Bag (WithPlayer Worker)))
  , _mapPathWorkers  :: !(Map PathId (WithPlayer Worker))
  , _mapRegionBonus  :: !(Map RegionId RegionBonus)
  , _mapProduces     :: !(Map CityId Resource)
  , _mapPrefected    :: ![ RegionId ]
  , marketLayout     :: ![ [ResourceCost] ]
  , _marketDeck      :: ![ Card ] -- ^ This includes the cards on display,
                                 -- they are at the front
  }

data RegionBonus = RegionBonus
  { _rbResource :: Maybe Resource
  , _rbMoney    :: Int
  }

data GameStatus = InProgress | EndTriggeredBy PlayerId | Finished {- Score -}

makeLenses ''PlayerState
makeLenses ''BoardState
makeLenses ''GameState
makeLenses ''RegionBonus

playerState :: PlayerId -> Lens' GameState PlayerState
playerState pid = lens ((Map.! pid) . _players) setP
  where
  setP s v = s { _players = Map.insert pid v (_players s) }






