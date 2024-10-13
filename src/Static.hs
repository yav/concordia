module Static where

import GHC.Generics(Generic)
import Data.Maybe(fromMaybe)
import Data.Text(Text)
import Data.Map(Map)
import Data.Map qualified as Map
import Data.Set(Set)
import Data.Set qualified as Set
import Optics
import Data.Aeson(ToJSON,FromJSON(parseJSON),(.:),ToJSONKey,FromJSONKey)
import Data.Aeson qualified as JS

import Types

newtype CityId = CityId Int
  deriving (Eq,Ord,Generic,Show,Read)
  deriving (ToJSON,FromJSON,ToJSONKey,FromJSONKey) via Int

newtype RegionId = RegionId Int
  deriving (Eq,Ord,Show,Read,Generic)
  deriving (ToJSON,FromJSON,ToJSONKey,FromJSONKey) via Int

newtype PathId = PathId Int
  deriving (Eq,Ord,Generic,Show,Read)
  deriving (ToJSON,FromJSON,ToJSONKey,FromJSONKey) via Int

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
  { _mapName         :: Text
  , _mapCities       :: Map CityId City
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

-- What workers can be built in a city
cityAccepts :: MapLayout -> Worker -> CityId  -> Bool
cityAccepts layout = \w cid -> cid `Set.member` citiesFor w
  where
  citiesFor w = Map.findWithDefault mempty w cached 
  cached = Map.fromList [ (w,setFor w) | w <- [ Person,Ship] ]
  setFor w = Map.keysSet
           $ Map.filter (not . null) 
           $ mapCityPaths w layout

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


instance FromJSON MapLayout where
  parseJSON = JS.withObject "Editor Map" \obj ->
    do name   <- obj .: "map"
       regObj <- obj .: "region"
       cities <- obj .: "city"
       paths  <- obj .: "edge"
       start  <- case [ cid | (cid,city) <- Map.toList cities
                            , city ^. cityTile == X ] of
                   [cid] -> pure cid
                   _ -> fail "No/multiple start cities"
       pure MapLayout
         { _mapName = name
         , _mapRegions = Map.keysSet (regObj :: Map RegionId Text)
         , _mapCities = cities
         , _mapPaths = paths
         , _mapStartCity = start
         }

instance FromJSON City where
  parseJSON = JS.withObject "City" \obj ->
    do tile <- obj .: "cityTile"
       reg  <- obj .: "cityRegion"
       pure City { _cityTile = tile, _cityRegion = reg }

instance FromJSON Path where
  parseJSON = JS.withObject "Path" \obj ->
    do w     <- obj .:  "pathWorker"
       city1 <- obj .:  "pathFrom"
       city2 <- obj .:  "pathTo"
       stop  <- obj JS..:? "pathCanStop"
       pure Path
         { _pathWorker  = w
         , _pathCanStop = fromMaybe True stop
         , _pathFrom    = city1
         , _pathTo      = city2
         }


