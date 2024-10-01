module Forum where

import Data.Maybe(isJust)
import Data.Text qualified as Text
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
     rs <- the (playerState pid % playerResources)
     let noCvt = (AskText "Pass", "Do not use Titus Valerius", pure ())
     askInputsMaybe_ pid "Convert resource" $
       noCvt :
       [ ( AskResource r
         , "Convert with Titus Valerius"
         , askInputsMaybe_ pid "New resource"
            [ ( AskTextResource "To" tgt
              , "Convert [" <> Text.pack (show r) <> "] to [" <> Text.pack (show tgt) <> "]"
              , do let loc = playerState pid % playerResources
                   updateThe_ loc (bagChange (-1) r)
                   updateThe_ loc (bagChange 1 tgt)
                   doLogBy' pid [T "Converted", G r, T "to", G tgt, T "(Titus Valerius)"]
              )
            | tgt <- if salt then [Salt] else [ Brick, Wheat, Tool, Wine, Cloth ]
            , tgt /= r
            ]
         )
       | r <- [ r | (r,_) <- bagToNumList rs, r /= Salt ]
       ]
