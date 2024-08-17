module Static where

import GHC.Generics(Generic)
import Data.Map(Map)
import Data.Map qualified as Map
import Data.Set
import Optics
import Data.Aeson(ToJSON,FromJSON)

import Types

newtype CityId = CityId Int
  deriving (Eq,Ord,Generic,Show,Read,ToJSON,FromJSON)

newtype RegionId = RegionId Int
  deriving (Eq,Ord,Generic,Show,Read,ToJSON,FromJSON)

newtype PathId = PathId Int
  deriving (Eq,Ord,Generic,Show,Read,ToJSON,FromJSON)

data City = City
  { _cityTile    :: CityTile
  , _cityRegion  :: RegionId
  }

data Path = Path
  { _pathWorker      :: Worker   -- ^ What workers we support
  , _pathCanStop     :: Bool     -- ^ For dotted path
  , _pathFrom        :: CityId   -- ^ Paths are not directed so from/to is arb.
  , _pathTo          :: CityId
  }

data MapLayout = MapLayout
  { _mapCities       :: Map CityId City
  , _mapRegions      :: Set RegionId
  , _mapPaths        :: Map PathId Path
  , _mapStartCity    :: CityId
  }


makeLenses ''City
makeLenses ''Path
makeLenses ''MapLayout

mapCityPaths :: Worker -> MapLayout -> Map CityId [PathId]
mapCityPaths w layout =
  Map.fromListWith (++)
    [ conn
    | (pathId, path) <- Map.toList (layout ^. mapPaths)
    , path ^. pathWorker == w
    , conn <- [ (path ^. pathFrom, [pathId]), (path ^. pathTo,[pathId]) ]
    ]

citiesInRegion :: MapLayout -> Map RegionId [CityId]
citiesInRegion layout =
  Map.fromListWith (++)
    [ (city ^. cityRegion, [cid])
    | (cid,city) <- Map.toList (layout ^. mapCities)
    ]

pathCities :: MapLayout -> PathId -> [CityId]
pathCities layout eid =
  case Map.lookup eid (layout ^. mapPaths) of
    Just path -> [ path ^. pathFrom, path ^. pathTo ]
    Nothing   -> []



