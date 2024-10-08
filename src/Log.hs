module Log where

import GHC.Generics(Generic)
import Data.Aeson(ToJSON)

import Data.Text(Text)
import Data.Text qualified as Text
import KOI.Basics(WithPlayer)
import Types ( Worker, Resource )
import Static ( CityId, PathId, RegionId )

data LogWord = T Text | G Resource | M | H | W (WithPlayer Worker) 
             | CID CityId | PID PathId | RID RegionId
             | L
  deriving(Generic,ToJSON)

tSh :: Show a => a -> LogWord
tSh x = T (Text.pack (show x))

