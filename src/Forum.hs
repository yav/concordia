module Forum where

import Data.Maybe(isJust,mapMaybe)
import Data.List(sortOn)
import Data.Text qualified as Text
import Data.Map qualified as Map
import Optics
import Data.Set()
import KOI.Basics
import KOI.Bag
import KOI
import Constants
import Types
import State
import Log
import Question
import HelperActions

gainForumTile :: PlayerId -> ForumTile -> Interact ()
gainForumTile pid t =
  do setThe (playerState pid % playerForumTiles % at t) (Just ())
     case t of
       Claudia -> onGainClaudia pid
       _       -> pure ()

hasForumTile :: PlayerId -> ForumTile -> Interact Bool
hasForumTile pid t = isJust <$> the (playerState pid % playerForumTiles % at t)

onGainClaudia :: PlayerId -> Interact ()
onGainClaudia pid =
  do updateThe_ (playerState pid % playerResourceLimit) (+4)
     doGainResources Nothing pid (bagFromList [Brick])

titusBonus :: PlayerId -> Interact ()
titusBonus pid =
  do salt <- the withSalt
     if salt then titusWithSalt else titusNoSalt

  where
  change from tgt =
    do let loc = playerState pid % playerResources
       updateThe_ loc (bagChange (-1) from)
       updateThe_ loc (bagChange 1 tgt)
       doLogBy' pid [T "Converted", G from, T "to", G tgt, T "(Titus Valerius)"]

  -- Convert cheapest resource to Salt (no reason not to?)
  titusWithSalt =
    do rs <- the (playerState pid % playerResources)
       let price (r,_) =
             do c <- Map.lookup r resourceCost
                pure (r,c)
       case sortOn snd (mapMaybe price (bagToNumList rs)) of
         [] -> pure ()
         (r,_) : _ -> change r Salt

  titusNoSalt =
    do rs <- the (playerState pid % playerResources)
       let noCvt = (AskText "Pass", "Do not use Titus Valerius", pure ())
       askInputsMaybe_ pid "Convert resource" $
         noCvt :
         [ ( AskResource r
           , "Convert with Titus Valerius"
           , askInputsMaybe_ pid "New resource"
              [ ( AskTextResource "To" tgt
                , "Convert [" <> Text.pack (show r) <> "] to [" <> Text.pack (show tgt) <> "]"
                , change r tgt
                )
              | tgt <- [ Brick, Wheat, Tool, Wine, Cloth ], tgt /= r
              ]
           )
         | r <- [ r | (r,_) <- bagToNumList rs, r /= Salt ]
         ]
