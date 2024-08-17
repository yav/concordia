module Constants where

import Data.Map(Map)
import Data.Map qualified as Map
import Types

workerCost :: [Resource]
workerCost = [ Wheat, Tool ]

resourceCost :: Map Resource Int
resourceCost = Map.fromList
  [ (Brick, 3)
  , (Wheat, 4)
  , (Tool,  5)
  , (Wine,  6)
  , (Cloth, 7)
  ]

resourcePrefectMoney :: Map Resource Int
resourcePrefectMoney = Map.fromList
  [ (Brick, 2)
  , (Wheat, 2)
  , (Tool,  2)
  , (Wine,  2)
  , (Cloth, 1)
  ]

cityCosts :: Map Resource (Int,[Resource])
cityCosts = Map.fromList
  [ (Brick, (1, [Wheat]))
  , (Wheat, (2, [Brick,Wheat]))
  , (Tool,  (3, [Brick,Tool]))
  , (Wine,  (4, [Brick,Wine]))
  , (Cloth, (5, [Brick,Cloth]))
  , (Salt,  (5, [Tool,Wine]))
  ]

