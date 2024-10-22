module State where

import Data.Map(Map)
import Data.Map qualified as Map
import Data.Set(Set)

import Optics
import KOI.Basics
import KOI.Bag
import KOI.RNG

import Types
import Static
import Log

data GameState = GameState
  { _players      :: !(Map PlayerId PlayerState)
  , _board        :: !BoardState
  , _curPlayer    :: !PlayerId
  , _playerOrder  :: ![PlayerId]
  , _playerDoubleBonus :: !PlayerId
  , _gameStatus   :: !GameStatus
  , _gameLog      :: ![[LogWord]]
  , _withSalt     :: !Bool
  , _forumSetup   :: !Bool
  , _rng          :: !RNG
  , _lastQuestion, _curQuestion :: !(Maybe PlayerId)
  }

data PlayerState = PlayerState
  { _playerWorkersForHire :: !(Bag Worker)
  , _playerHousesToBuild  :: !Int
  , _playerResources      :: !(Bag Resource)
  , _playerMoney          :: !Int
  , _playerHand           :: ![Card]
  , _playerDiscard        :: ![Card]   -- ^ Most recent first
  , _playerResourceLimit  :: !Int      -- ^ Including workers
  , _playerForumTiles     :: !(Set ForumTile)
  }

data BoardState = BoardState
  { _mapLayout       :: !MapLayout
  , _mapHouses       :: !(Map CityId [PlayerId])
  , _mapCityWorkers  :: !(Map CityId (Bag (WithPlayer Worker)))
  , _mapPathWorkers  :: !(Map PathId (WithPlayer Worker))
  , _mapRegionBonus  :: !(Map RegionId RegionBonus)
  , _mapExtraMoneyBonus :: !Int
  , _mapProduces     :: !(Map CityId Resource)
  , _mapPrefected    :: ![ RegionId ]
  , _marketLayout    :: ![ [ResourceCost] ]
  , _marketDeck      :: ![ Card ] -- ^ This includes the cards on display,
                                 -- they are at the front
  , _forumMarket :: ![ForumTile]
  , _forumDiscard :: ![ForumTile]
  }

data RegionBonus = RegionBonus
  { _rbResource :: BonusChoice
  , _rbMoney    :: Int
  }

data BonusChoice = NoBonus | VariableBonus | ResourceBonus Resource


data GameStatus = InProgress | EndTriggeredBy PlayerId | Finished PlayerId

makeLenses ''PlayerState
makeLenses ''BoardState
makeLenses ''GameState
makeLenses ''RegionBonus

playerState :: PlayerId -> Lens' GameState PlayerState
playerState pid = lens ((Map.! pid) . _players) setP
  where
  setP s v = s { _players = Map.insert pid v (_players s) }

