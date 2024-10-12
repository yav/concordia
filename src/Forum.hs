module Forum where

import Data.Text(Text)
import Data.Text qualified as Text
import Data.Map qualified as Map
import Optics
import Data.Set()
import KOI.Basics
import KOI.Bag
import KOI
import Types
import State
import Log
import Question
import HelperActions
import Static

gainForumTile :: PlayerId -> ForumTile -> Interact ()
gainForumTile pid t =
  do setThe (playerState pid % playerForumTiles % at t) (Just ())
     case t of
       Claudia -> onGainClaudia pid
       _       -> pure ()

onGainClaudia :: PlayerId -> Interact ()
onGainClaudia pid =
  do updateThe_ (playerState pid % playerResourceLimit) (+4)
     doGainResources Nothing pid (bagFromList [Brick])

doConvertResourceToSalt :: Text -> PlayerId -> Int -> Int -> Interact ()
doConvertResourceToSalt who pid count tot
  | count <= 0 || count > tot = pure ()
  | otherwise =
  do salt <- the withSalt
     rs <- the (playerState pid % playerResources)
     let noCvt = (AskText "Done", "Finished with " <> who, pure ())
     let suff = if tot == 1 then "" else Text.pack (" " <> show count <> "/" <> show tot)
     askInputsMaybe_ pid ("Convert resource" <> suff) $
       noCvt :
       [ ( AskResource r
         , "Convert with " <> who
         , askInputsMaybe_ pid "New resource"
            [ ( AskTextResource "To" tgt
              , "Convert [" <> Text.pack (show r) <> "] to [" <> Text.pack (show tgt) <> "]"
              , do let loc = playerState pid % playerResources
                   updateThe_ loc (bagChange (-1) r)
                   updateThe_ loc (bagChange 1 tgt)
                   doLogBy' pid [T "Converted", G r, T "to", G tgt, T ("(" <> who <> ")")]
                   doConvertResourceToSalt who pid (count + 1) tot
              )
            | tgt <- if salt then [Salt] else normalResources
            , tgt /= r
            ]
         )
       | r <- [ r | (r,_) <- bagToNumList rs, r /= Salt ]
       ]


titusBonus :: PlayerId -> Interact ()
titusBonus pid = doConvertResourceToSalt "Titus Valerius" pid 1 1

actJulius :: PlayerId -> Interact ()
actJulius pid =
  do doLogBy' pid [T "Julius"]
     doMoveWorkers pid

actNumerius :: PlayerId -> Interact ()
actNumerius pid =
  do brd <- the board
     let static = brd ^. mapLayout
         houses c = Map.findWithDefault [] c (brd ^. mapHouses)
         hasHouseIn c = pid `elem` houses c
         add tot cities = if any hasHouseIn cities then 2 + tot else tot
         earnings = Map.foldl' add 0 (citiesInRegion static)
     doChangeMoney pid earnings
     doLogBy' pid [T "Earned", tSh earnings, M, T " (Numerius)"]

actSpurius :: PlayerId -> Interact ()
actSpurius pid =
  do salt <- the withSalt
     good <- if salt then pure Salt else
       askInputs pid "Choose good (Spurius)"
         [ (AskResource r, "Gain this resource", pure r)
         | r <- normalResources
         ]
     doGainResources (Just "Spurius") pid (bagFromList [good])

actAugustuts :: PlayerId -> Interact ()
actAugustuts pid = doConvertResourceToSalt "Augustus" pid 1 2

actLaurentius :: PlayerId -> Interact ()
actLaurentius pid =
  doGainResources (Just "Laurentius") pid (bagFromList [Brick,Wheat])
