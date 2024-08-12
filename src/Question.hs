module Question where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON,FromJSON)

data Question = Button Int
  deriving (Show,Read,Eq,Ord,Generic,FromJSON,ToJSON)

