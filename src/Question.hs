module Question where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON,FromJSON)
import Types

data Question =
    AskWorker Worker
  | Pass
  deriving (Show,Read,Eq,Ord,Generic,FromJSON,ToJSON)

