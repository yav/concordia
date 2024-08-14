module Question where

import GHC.Generics(Generic)
import Data.Text(Text)
import Data.Aeson(ToJSON,FromJSON)
import Types
import Static

data Question =
    AskWorker Worker
  | AskCity CityId
  | AskRegion RegionId
  | AskPath PathId
  | AskText Text
  deriving (Show,Read,Eq,Ord,Generic,FromJSON,ToJSON)

