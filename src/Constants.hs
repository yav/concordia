module Constants where

import Data.Map(Map)
import Data.Map qualified as Map
import Types

workerCost :: [Resource]
workerCost = [ Wheat, Tools ]

resourceCost :: Map Resource Int
resourceCost = Map.fromList
  [ (Brick, 3)
  , (Wheat, 4)
  , (Tools, 5)
  , (Wine,  6)
  , (Cloth, 7)
  ]

resourcePrefectMoney :: Map Resource Int
resourcePrefectMoney = Map.fromList
  [ (Brick, 2)
  , (Wheat, 2)
  , (Tools, 2)
  , (Wine,  2)
  , (Cloth, 1)
  ]

