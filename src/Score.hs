module Score where

import Data.Set qualified as Set
import Data.Map qualified as Map
import Optics
import KOI.Basics(PlayerId, WithPlayer(..))
import KOI.Bag
import Types
import State
import Static
import Constants


godValue :: PlayerId -> GameState -> God -> Int
godValue pid gs god =
  let ps = gs ^. playerState pid
      brd = gs ^. board
      team = Nothing
      have = [ r | c <- ps ^. playerHand ++ ps ^. playerDiscard 
                 , Minerva r <- cardColor c ]
  in
  case god of
    Vesta -> vesta ps
    Jupiter -> jupiter pid brd
    Saturnus -> saturnus pid brd
    Mercurius -> mercurious pid brd
    Mars -> mars pid brd
    Minerva r -> specialist have r pid brd
    Venus -> venus pid team brd

vesta :: PlayerState -> Int
vesta ps = div (goods + ps ^. playerMoney) 10
  where
  cost = Map.insert Salt (maximum resourceCost) resourceCost
  goods = sum [ n * c
              | (r,n) <- bagToNumList (ps ^. playerResources)
              , let c = Map.findWithDefault 0 r cost
              ]

jupiter :: PlayerId -> BoardState -> Int
jupiter pid b = Map.size (Map.filterWithKey isOurs (b ^. mapHouses))
  where
  isOurs c ps = not (isBrick c) && pid `elem` ps
  isBrick c = Map.lookup c (b ^. mapProduces) == Just Brick

saturnus :: PlayerId -> BoardState -> Int
saturnus pid b = Map.size (Map.filter (any present) regions) 
  where
  present c = c `Map.member` cities
  cities    = Map.filter (pid `elem`) (b ^. mapHouses)
  regions   = citiesInRegion (b ^. mapLayout)

mercurious :: PlayerId -> BoardState -> Int
mercurious  pid b = 2 * Set.size tys
  where
  cities = Map.filter (pid `elem`) (b ^. mapHouses)
  prod   = Map.intersectionWith (\_ p -> p) cities (b ^. mapProduces)
  tys    = Set.delete Salt (Set.fromList (Map.elems prod))

mars :: PlayerId -> BoardState -> Int
mars pid b = 2 * (inCities + onPaths)
  where
  isUs (p :-> _) = p == pid
  onPaths = Map.size (Map.filter isUs (b ^. mapPathWorkers))
  inCities = sum (count <$> b ^. mapCityWorkers)
  count ba = sum [ n | (p,n) <- bagToNumList ba, isUs p ]

specialist :: [Resource] -> Resource -> PlayerId -> BoardState -> Int
specialist allSpecialists r pid b = val * Map.size prod
  where
  ours      = Map.filter (pid `elem`) (b ^. mapHouses)
  match r1  = r1 == r || countSalt && r1 == Salt
  resource  = Map.filter match (b ^. mapProduces)
  prod      = Map.intersectionWith (\_ _ -> ()) ours resource
  val       = if r == Cloth || r == Wine then 4 else 3
  countSalt = foldr (\r1 no -> if r1 `elem` allSpecialists then r == r1 else no)
                    False
                    [Cloth,Wine,Tool,Wheat,Brick]

venus :: PlayerId -> Maybe PlayerId -> BoardState -> Int
venus pid team b = val * Map.size (Map.filter consider regions) 
  where
  consider cs = let ourH = filter (`Map.member` ours) cs
                    theirH = filter (`Map.member` theirs) cs
                in case team of
                     Nothing -> length ourH >= 2
                     Just _  -> not (null ourH) && not (null theirH)
  ours      = Map.filter (pid `elem`) (b ^. mapHouses)
  theirs    = case team of
                Just partner -> Map.filter (partner `elem`) (b ^. mapHouses)
                Nothing -> mempty
  regions   = citiesInRegion (b ^. mapLayout)
  val       = case team of
                Just _  -> 1
                Nothing -> 2


