module Static where

import GHC.Generics(Generic)
import Data.Map(Map)
import Data.Map qualified as Map
import Data.Set
import Data.Aeson(ToJSON,FromJSON)

import Types

newtype CityId = CityId Int
  deriving (Eq,Ord,Generic,Show,Read,ToJSON,FromJSON)

newtype RegionId = RegionId Int
  deriving (Eq,Ord,Generic,Show,Read,ToJSON,FromJSON)

newtype PathId = PathId Int
  deriving (Eq,Ord,Generic,Show,Read,ToJSON,FromJSON)

data City = City
  { cityTile    :: CityTile
  , cityRegion  :: RegionId
  }

data Path = Path
  { pathWorker      :: Worker   -- ^ What workers we support
  , pathCanStop     :: Bool     -- ^ For dotted path
  , pathFrom        :: CityId   -- ^ Paths are not directed so from/to is arb.
  , pathTo          :: CityId
  }

data MapLayout = MapLayout
  { mapCities       :: Map CityId City
  , mapRegions      :: Set RegionId
  , mapPaths        :: Map PathId Path
  , mapStartCity    :: CityId
  }

mapCityPaths :: MapLayout -> Map CityId [PathId]
mapCityPaths layout =
  Map.fromListWith (++)
    [ conn
    | (pathId, path) <- Map.toList (mapPaths layout)
    , conn <- [ (pathFrom path,[pathId]), (pathTo path,[pathId]) ]
    ]






