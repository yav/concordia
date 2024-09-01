module Cards where

import Types

tribune :: Card
tribune = Card [Tribune] [] [Mars]

basicMercator :: Card
basicMercator = Card [Mercator 3] [] [Mercurius]

advMercator :: Card
advMercator = Card [Mercator 5] [Wine] [Mercurius]

basicArchitect :: Card
basicArchitect = Card [Architect] [] [Jupiter]

architect :: Card
architect = Card [Architect] [Tool] [Jupiter]

diplomat, diplomat1, diplomat3, diplomat4, diplomat5 :: Card
diplomat = Card [Diplomat] [] [Jupiter]
diplomat1 = Card [Diplomat] [Tool] [Saturnus]
diplomat3 = Card [Diplomat] [Wheat] [Saturnus]
diplomat4 = Card [Diplomat] [Tool] [Mercurius]
diplomat5 = Card [Diplomat] [Wheat] [Mars]


basicPrefect :: Card
basicPrefect = Card [Prefect] [] [Saturnus]

prefect :: Card
prefect = Card [Prefect] [Wine] [Saturnus]

senator :: Card
senator = Card [Senator] [] [Vesta]


specialistMason :: Card
specialistMason = Card [Specialist Brick] [Wheat] [Minerva]

specialistFarmer :: Card
specialistFarmer =
  Card [Specialist Wheat] [Brick, Wheat] [Minerva]

specialistSmith :: Card
specialistSmith =
  Card [Specialist Tool] [Brick, Tool] [Minerva]

specialistVintner :: Card
specialistVintner = Card [Specialist Wine] [Brick, Wine] [Minerva]

specialistWeaver :: Card
specialistWeaver = Card [Specialist Cloth] [Brick, Cloth] [Minerva]

consul :: Card
consul = Card [Consul] [Cloth] [Jupiter]

colonist :: Card
colonist = Card [Colonist Settle, Colonist Tax] [Wheat] [Mars]

magister :: Card
magister = Card [Magister] [] [Venus]

legatus :: Card
legatus = Card [Legatus] [] [Jupiter,Saturnus,Venus]

prefectArchitect :: Card
prefectArchitect = Card [Prefect,Architect] [Wine] [Venus]

architectMercator :: Card
architectMercator = Card [Architect,Mercator 5] [Tool] [Venus]

prefectMercator :: Card
prefectMercator = Card [Prefect,Mercator 5] [Wine] [Venus]


startDeckBase, startDeckVenus,
  startDeckVenusTeam1, startDeckVenusTeam2 :: [Card]
startDeckBase =
  [ tribune, basicPrefect, basicPrefect, diplomat,
    basicArchitect, basicMercator, senator ]

startDeckVenus = magister : startDeckBase

startDeckVenusTeam1 =
  [ tribune, basicPrefect, basicArchitect, basicMercator, senator, legatus ]

startDeckVenusTeam2 =
  [ tribune, basicPrefect, diplomat, basicMercator, senator, legatus ]
 

deck1, deck2, deck3, deck4, deck5 :: Bool -> [Card]

deck1 isBase
  | isBase = prefect : specialistMason : common
  | otherwise = prefectArchitect : common
  where
  common = [ advMercator, architect, colonist, diplomat1,
               specialistMason, specialistFarmer, specialistSmith]

deck2 isBase
  | isBase = architect : common
  | otherwise = architectMercator : specialistMason : common
  where
  common = [ advMercator, colonist, prefect, consul,
          specialistVintner, specialistWeaver]

deck3 isBase
  | isBase = advMercator : common
  | otherwise  = prefectMercator : common
  where
  common = [ architect, colonist, prefect, consul, diplomat3]

deck4 isBase
  | isBase = architect : common
  | otherwise = architectMercator : common
  where common = [architect, colonist, prefect, consul, diplomat4]

deck5 _isBase = [ advMercator, prefect, consul, diplomat5]

marketDeck :: Bool -> [[Card]]
marketDeck isBase = map ($ isBase) [deck1, deck2, deck3, deck4, deck5]
