module Question where

import GHC.Generics(Generic)
import Data.Text(Text)
import Data.Aeson(ToJSON,FromJSON)
import KOI.Basics(PlayerId)
import Types
import Static

data Question =
    AskWorker Worker
  | AskCityWorker CityId Worker
  | AskResource Resource
  | AskTextResource Text Resource
  | AskCity CityId
  | AskRegion RegionId
  | AskPath PathId
  | AskMarket Int
  | AskForumMarket Int
  | AskMarketAct Int Int -- ^ Market card, action number
  | AskHand Int Int -- ^ Card, Action Number
  | AskHandForum Int
  | AskDiscard PlayerId Int -- ^ Player's discard, action number
  | AskText Text
  deriving (Show,Read,Eq,Ord,Generic,FromJSON,ToJSON)

