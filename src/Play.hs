module Play where

import Control.Monad(when)
import Optics
import KOI.Basics
import KOI.Bag
import KOI
import State
import Types

play :: Interact ()
play =
  do s <- getState
     case s ^. gameStatus of
       Finished {} -> pure ()
       EndTriggeredBy pid
         | pid == s ^. curPlayer ->
           setThe gameStatus Finished
         | otherwise -> doTakeTurn >> nextPlayer >> play
       InProgress -> doTakeTurn >> checkEndGame >> nextPlayer >> play

-- Assumes InProgress
checkEndGame :: Interact ()
checkEndGame =
  do s <- getState
     let playerDone p = p ^. playerHousesToBuild == 0
     when (null (s ^. board % marketDeck) || any playerDone (s ^. players))
          (setThe gameStatus (EndTriggeredBy (s ^. curPlayer))) -- Get points


nextPlayer :: Interact ()
nextPlayer =
  do s <- getState
     case s ^. nextPlayers of
       x : xs ->
         do setThe curPlayer x
            setThe nextPlayers xs
            setThe prevPlayers (s ^. curPlayer : s ^. prevPlayers)
       [] ->
         case reverse (s ^. prevPlayers) of
           x : xs ->
             do setThe curPlayer x
                setThe nextPlayers xs
                setThe prevPlayers []
           [] -> pure ()

doTakeTurn :: Interact ()
doTakeTurn = pure ()

{-
doTribune :: Interact ()
doTribune =
  do s <- getState
     let pid       = s ^. curPlayer
         pstate    = s ^. players % at pid
         discard   = pstate ^. playerDiscard
         money     = max 0 (length discard - 3)
         resources =
     undefined
-}

canBuildWorker :: PlayerId -> Interact [Worker]
canBuildWorker pid =
  do mbPstate <- the (players % at pid)
     case mbPstate of
       Nothing -> pure []
       Just pstate -> pure [ w | canPay, w <- [Person,Ship], hasW w ]
         where
         workers   = pstate ^. playerWorkersForHire
         hasW p    = bagContains p workers > 0
         resources = pstate ^. playerResources
         hasR p    = bagContains p resources > 0
         canPay    = hasR Wheat && hasR Iron


{-
askBuildWorker :: [Worker] -> Interact ()
askBuildWorker =
  askInputsMaybe_
-}






