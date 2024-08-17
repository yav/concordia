module Question where

import GHC.Generics(Generic)
import Data.Text(Text)
import Data.Aeson(ToJSON,FromJSON)
import Types
import Static

data Question =
    AskWorker Worker
  | AskCityWorker CityId Worker
  | AskResource Resource
  | AskCity CityId
  | AskRegion RegionId
  | AskPath PathId
  | AskMarket Int
  | AskText Text
  deriving (Show,Read,Eq,Ord,Generic,FromJSON,ToJSON)

