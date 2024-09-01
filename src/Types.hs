module Types where

import GHC.Generics(Generic)
import Data.Text(Text)
import Data.Text qualified as Text
import Data.Aeson(ToJSON,FromJSON)


data Resource = Brick | Wheat | Tool | Wine | Cloth | Salt
  deriving (Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

data ResourceCost = Resource Resource | Any
  deriving (Eq,Generic,ToJSON)

data Worker   = Person | Ship
  deriving (Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

data CityTile = A | B | C | D | X
  deriving (Eq,Ord,Generic,FromJSON)

data Action   = Architect 
              | Senator -- "pretor" in team
              | Prefect | Diplomat | Tribune
              | Colonist ColonistAction
              | Mercator Int | Specialist Resource
              | Magister 
              | Consul  -- "proconsul" in team
              | Legatus -- only team
  deriving (Eq,Show,Generic,ToJSON)

actionText :: Action -> Text
actionText act =
  case act of
    Mercator n -> "Mercator (" <> Text.pack (show n) <> ")"
    Colonist c ->
      case c of
        Settle -> "Colonist Settle"
        Tax    -> "Colonist Tax"
    _ -> Text.pack (show act)

data ColonistAction = Settle | Tax
  deriving (Eq,Show,Generic,ToJSON)

data God      = Vesta | Jupiter | Saturnus | Venus | Mercurius | Mars | Minerva
  deriving (Generic,ToJSON)

data Card     = Card { cardActions :: [Action]
                     , cardCost    :: [Resource]
                     , cardColor   :: [God]
                     }
  deriving (Generic,ToJSON)






