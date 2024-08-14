module Cards where

import Types

tribune :: Card
tribune = Card [Tribune] [] [Mars]

basic_mercator :: Card
basic_mercator = Card [Mercator 3] [] [Mercurius]

colonist :: Card
colonist = Card [Colonist] [Resource Wheat] [Mars]




