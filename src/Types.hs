module Types where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON,FromJSON)

data Resource = Brick | Wheat | Iron | Wine | Cloth
  deriving (Eq,Ord)

data ResourceCost = Resource Resource | Any

data Worker   = Person | Ship
  deriving (Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

data CityTile = A | B | C | D | E | Capital
  deriving (Eq,Ord)

data Action   = Architect | Senator | Prefect | Diplomat | Tribune
              | Mercator Int | Specialist Resource

data Color    = Purple | Blue | Yellow | Brown | Orange | LightGreen

data Card     = Card { cardActions :: [Action]
                     , cardCost    :: [ResourceCost]
                     , cardColor   :: Color
                     }






