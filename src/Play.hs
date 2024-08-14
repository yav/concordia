module Play where

import Control.Monad(when)
import Data.Maybe(fromMaybe)
import Optics
import KOI.Basics
import KOI.Bag
import KOI
import Static
import State
import Types
import Question

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

-- | Change the money by this much.
doChangeMoney :: PlayerId -> Int -> Interact ()
doChangeMoney pid amt
  | amt == 0 = pure ()
  | otherwise = updateThe_ (playerState pid % playerMoney) (max 0 . (+ amt))

-- | Add some cards to a player's hand
doAddCards :: PlayerId -> [Card] -> Interact ()
doAddCards pid cs = updateThe_ (playerState pid % playerHand) (cs ++)

-- | Pay for a worker and place it on the board
doBuildWorker :: PlayerId -> Worker -> CityId -> Interact ()
doBuildWorker pid w city =
  do updateThe_ (playerState pid % playerResources)
                (bagChange (-1) Wheat . bagChange (-1) Iron)
     updateThe_ (playerState pid % playerWorkersForHire)
                (bagChange (-1) w)
     updateThe_ (board % mapCityWorkers % at city)
                (Just . bagChange 1 (pid :-> w) . fromMaybe bagEmpty)

-- | What kind of worker can this player build at the moment
-- (i.e., they have the worker and the resources to build it).
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




actTribune :: Interact ()
actTribune =
  do pid <- the curPlayer
     discard <- updateThe (playerState pid % playerDiscard) (\d -> (d, []))
     doChangeMoney pid (length discard + 1 - 3) -- assuming tribune is not in discard
     doAddCards pid discard
     ws <- canBuildWorker pid
     capital <- mapStartCity . mapLayout <$> the board
     askInputsMaybe_ "Would you like to build a worker?" $
        (pid :-> Pass, "Do not build worker.", pure ())
      : [ (pid :-> AskWorker w, "Build a worker.",
           doBuildWorker pid w capital)
        | w <- ws
        ]








