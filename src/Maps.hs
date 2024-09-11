module Maps where

import Text.Heredoc
import Data.Aeson qualified as JS
import Static

data Maps = Italia | Ionium | Crete | Cyprus | Hellas | Hispania | Imperium
  deriving (Eq,Ord,Show,Bounded,Enum)


mapData :: Maps -> MapLayout
mapData mp =
  case res of
    Right a -> a
    Left err -> error err
  where
  res =
    JS.eitherDecode
      case mp of
        Italia   -> [there|ui/maps/Italia.json|]
        Ionium   -> [there|ui/maps/Ionium.json|]
        Crete    -> [there|ui/maps/Crete.json|]
        Cyprus   -> [there|ui/maps/Cyprus.json|]
        Hellas   -> [there|ui/maps/Hellas.json|]
        Hispania -> [there|ui/maps/Hispania.json|]
        Imperium -> [there|ui/maps/Imperium.json|]