module Types where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON,FromJSON)

data Resource = Brick | Wheat | Tools | Wine | Cloth | Salt
  deriving (Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

data ResourceCost = Resource Resource | Any

data Worker   = Person | Ship
  deriving (Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

data CityTile = A | B | C | D | E | Capital
  deriving (Eq,Ord)

data Action   = Architect | Senator | Prefect | Diplomat | Tribune
              | Colonist
              | Mercator Int | Specialist Resource
              | Magister | Consul

data God      = Vesta | Jupiter | Saturnus | Venus | Mercurius | Mars | Minerva

data Card     = Card { cardActions :: [Action]
                     , cardCost    :: [Resource]
                     , cardColor   :: [God]
                     }






