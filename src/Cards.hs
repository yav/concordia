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

magister :: Card
magister = Card [Magister] [] [Venus]

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
colonist = Card [Colonist] [Wheat] [Mars]

startDeck, deck1, deck2, deck3, deck4, deck5 :: [Card]
startDeck =
  [tribune, basicPrefect, basicPrefect, diplomat,
    basicArchitect, basicMercator, senator]

deck1 = [ advMercator, architect, colonist, prefect, diplomat1,
          specialistMason, specialistFarmer, specialistSmith]

deck2 = [ advMercator, architect, colonist, prefect, consul,
          specialistVintner, specialistWeaver]

deck3 = [ advMercator, architect, colonist, prefect, consul, diplomat3]

deck4 = [architect, colonist, prefect, consul, diplomat4]

deck5 = [ advMercator, prefect, consul, diplomat5]

marketDeck :: [[Card]]
marketDeck = [deck1, deck2, deck3, deck4, deck5]
