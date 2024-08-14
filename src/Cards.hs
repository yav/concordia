module Cards where

import Types

tribune :: Card
tribune = Card [Tribune] [] [Mars]

basic_mercator :: Card
basic_mercator = Card [Mercator 3] [] [Mercurius]

adv_mercator :: Card
adv_mercator = Card [Mercator 5] [Resource Wine] [Mercurius]

architect :: Card
architect = Card [Architect] [Resource Tools] [Jupiter]

diplomat :: Card
diplomat = Card [Diplomat] [] [Jupiter]
diplomat1 = Card [Diplomat] [Resource Tools] [Saturn]
diplomat3 = Card [Diplomat] [Resource Wheat] [Saturn]
diplomat4 = Card [Diplomat] [Resource Tools] [Mercurius]
diplomat5 = Card [Diplomat] [Resource Wheat] [Mars]

prefect :: Card
prefect = Card [Prefect] [Resource Wine] [Saturnus]

senator :: Card
senator = Card [Senator] [] [Vesta]

magister :: Card
magister = Card [Magister] [] [Venus]

specialist_mason :: Card
specialist_mason = Card [Specialist Brick] [Resource Wheat] [Minerva]

specialist_farmer :: Card
specialist_farmer = Card [Specialist Wheat] [Resource Brick, Resource Wheat] [Minerva]

specialist_smith :: Card
specialist_smith = Card [Specialist Tools] [Resource Brick, Resource Tools] [Minerva]

specialist_vintner :: Card
specialist_vintner = Card [Specialist Wine] [Resource Brick, Resource Wine] [Minerva]

specialist_weaver :: Card
specialist_weaver = Card [Specialist Cloth] [Resource Brick, Resource Cloth] [Minerva]

consul :: Card
consul = Card [Consul] [Resource Cloth] [Jupiter]

colonist :: Card
colonist = Card [Colonist] [Resource Wheat] [Mars]




