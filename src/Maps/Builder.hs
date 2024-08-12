module Maps.Builder where

import Data.Text(Text)
import Data.Map qualified as Map
import Control.Monad(liftM,ap)

import Static
import Types

newtype Builder a = Builder (MapLayout -> (a, MapLayout))

build :: Builder () -> MapLayout
build (Builder m) = snd (m empty)
  where
  empty = MapLayout mempty mempty mempty (CityId 0)

instance Functor Builder where
  fmap = liftM

instance Applicative Builder where
  pure a = Builder \s -> (a,s)
  (<*>)  = ap

instance Monad Builder where
  Builder m >>= k = Builder \s ->
    let (a,s1)    = m s
        Builder m2 = k a
    in m2 s1

region :: Text -> Builder ()
region y = Builder \s ->
  let x = mapRegionName s
      n = RegionId (Map.size x)
  in ((), s { mapRegionName = Map.insert n y x })

city :: Text -> CityTile -> Builder CityId
city name tile = Builder \s ->
  let x = mapCities s
      n = CityId (Map.size x)
      y = City name tile (case Map.minViewWithKey (mapRegionName s) of
                            Just ((r,_),_) -> r
                            Nothing -> error "No region")
  in (n, s { mapCities = Map.insert n y x
           , mapStartCity = case tile of
                              Capital -> n
                              _       -> mapStartCity s })

path :: Worker -> Bool -> CityId -> CityId -> Builder ()
path w dot from to = Builder \s ->
  let x = mapPaths s
      n = PathId (Map.size x)
      y = Path w dot from to
  in ((), s { mapPaths = Map.insert n y x})

landPath :: CityId -> CityId -> Builder ()
landPath = path Person True

waterPath :: CityId -> CityId -> Builder ()
waterPath = path Ship True


