module Score where

import Data.Map(Map)
import Data.Map qualified as Map
import Data.Maybe(listToMaybe)
import Optics
import KOI.Basics(PlayerId, WithPlayer(..))
import KOI.Bag
import Types
import State
import Static
import Constants





godValue :: PlayerId -> Maybe PlayerId -> GameState -> Bool -> God -> Int
godValue pid mbTeam gs =
  let ps = gs ^. playerState pid
      brd = gs ^. board
      specs = [ r | c <- ps ^. playerHand ++ ps ^. playerDiscard 
                          , Minerva r <- cardColor c ]
      team = (`countRegions` brd) <$> mbTeam
      cities = countCities pid brd
      provinces = countRegions pid brd
  in \inMarket god ->
  let
    salt = saltSpecialist
           case god of
             Minerva r | inMarket -> r : specs
             _ -> specs
  in              
  case god of
    Vesta -> vesta ps
    Jupiter -> jupiter cities
    Saturnus -> saturnus provinces
    Mercurius -> mercurious cities
    Mars -> mars pid brd
    Minerva r -> specialist r salt cities
    Venus -> venus provinces team


-- | How many cities we have for various resource types. Note that it is
-- possible to miss a resource entry (meaning 0) or have an entry with 0.
countCities :: PlayerId -> BoardState -> Map Resource Int
countCities pid b = Map.fromListWith (+) (Map.elems resources)
  where
  resources = Map.intersectionWith mk (b ^. mapProduces) (b ^. mapHouses)
  mk r hs = (r, if pid `elem` hs then 1 else 0)

-- | How many houses we have in each region
countRegions :: PlayerId -> BoardState -> Map RegionId Int
countRegions pid b = sum . map presentIn <$> citiesInRegion (b ^. mapLayout)
  where
  presentIn c =
    case Map.lookup c (b ^. mapHouses) of
      Just ps | pid `elem` ps -> 1
      _ -> 0



-- | What resource to count salt 
saltSpecialist :: [Resource] -> Maybe Resource
saltSpecialist have = listToMaybe [ r | r <- reverse normalResources, r `elem` have ]

vesta :: PlayerState -> Int
vesta ps = div (goods + ps ^. playerMoney) 10
  where
  cost = Map.insert Salt (maximum resourceCost) resourceCost
  goods = sum [ n * c
              | (r,n) <- bagToNumList (ps ^. playerResources)
              , let c = Map.findWithDefault 0 r cost
              ]

jupiter :: Map Resource Int -> Int
jupiter = sum . Map.delete Brick

saturnus :: Map RegionId Int -> Int
saturnus = Map.size . Map.filter (> 0) 

mercurious :: Map Resource Int -> Int
mercurious = (2*) . Map.size . Map.filter (> 0) . Map.delete Salt

mars :: PlayerId -> BoardState -> Int
mars pid b = 2 * (inCities + onPaths)
  where
  isUs (p :-> _) = p == pid
  onPaths = Map.size (Map.filter isUs (b ^. mapPathWorkers))
  inCities = sum (count <$> b ^. mapCityWorkers)
  count ba = sum [ n | (p,n) <- bagToNumList ba, isUs p ]

specialist :: Resource -> Maybe Resource -> Map Resource Int -> Int
specialist r salt cities = val * (res r + fromSalt)
  where
  res x    = Map.findWithDefault 0 x cities
  fromSalt = if salt == Just r then res Salt else 0

  val = case r of
          Cloth -> 5
          Wine  -> 4
          _     -> 3

venus :: Map RegionId Int -> Maybe (Map RegionId Int) -> Int
venus ours mbTeam =
  case mbTeam of
    Nothing -> 2 * Map.size (Map.filter (>= 2) ours)
    Just team ->
      Map.size (Map.filter id (Map.intersectionWith (\a b -> a > 0 && b > 0) ours team))



