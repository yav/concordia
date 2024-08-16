module Cards where

import Types

tribune :: Card
tribune = Card [Tribune] [] [Mars]

basic_mercator :: Card
basic_mercator = Card [Mercator 3] [] [Mercurius]

adv_mercator :: Card
adv_mercator = Card [Mercator 5] [Wine] [Mercurius]

architect :: Card
architect = Card [Architect] [Tools] [Jupiter]

diplomat, diplomat1, diplomat3, diplomat4, diplomat5 :: Card
diplomat = Card [Diplomat] [] [Jupiter]
diplomat1 = Card [Diplomat] [Tools] [Saturnus]
diplomat3 = Card [Diplomat] [Wheat] [Saturnus]
diplomat4 = Card [Diplomat] [Tools] [Mercurius]
diplomat5 = Card [Diplomat] [Wheat] [Mars]

prefect :: Card
prefect = Card [Prefect] [Wine] [Saturnus]

senator :: Card
senator = Card [Senator] [] [Vesta]

magister :: Card
magister = Card [Magister] [] [Venus]

specialist_mason :: Card
specialist_mason = Card [Specialist Brick] [Wheat] [Minerva]

specialist_farmer :: Card
specialist_farmer =
  Card [Specialist Wheat] [Brick, Wheat] [Minerva]

specialist_smith :: Card
specialist_smith =
  Card [Specialist Tools] [Brick, Tools] [Minerva]

specialist_vintner :: Card
specialist_vintner = Card [Specialist Wine] [Brick, Wine] [Minerva]

specialist_weaver :: Card
specialist_weaver = Card [Specialist Cloth] [Brick, Cloth] [Minerva]

consul :: Card
consul = Card [Consul] [Cloth] [Jupiter]

colonist :: Card
colonist = Card [Colonist] [Wheat] [Mars]

start_deck = [tribune, prefect, prefect, diplomat, architect, basic_mercator, senator]
deck1 = [adv_mercator, architect, colonist, prefect, diplomat1, specialist_mason, specialist_farmer, specialist_smith]
deck2 = [adv_mercator, architect, colonist, prefect, consul, specialist_vintner, specialist_weaver]
deck3 = [adv_mercator, architect, colonist, prefect, consul, diplomat3]
deck4 = [architect, colonist, prefect, consul, diplomat4]
deck5 = [adv_mercator, prefect, consul, diplomat5]

market_deck = [deck1, deck2, deck3, deck4, deck5]
