module Static where

import Data.Text(Text)
import Data.Map(Map)
import Data.Map qualified as Map

import Types

newtype CityId = CityId Int
  deriving (Eq,Ord)

newtype RegionId = RegionId Int
  deriving (Eq,Ord)

newtype PathId = PathId Int
  deriving (Eq,Ord)

data City = City
  { cityName    :: Text
  , cityTile    :: CityTile
  , cityRegion  :: RegionId
  }

data Path = Path
  { pathWorker      :: Worker   -- ^ Waht workers we support
  , pathCanStop     :: Bool     -- ^ for dotted path
  , pathFrom        :: CityId   -- ^ Paths are not directoed so from/to is arb.
  , pathTo          :: CityId
  }

data MapLayout = MapLayout
  { mapCities       :: Map CityId City
  , mapRegionName   :: Map RegionId Text
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






