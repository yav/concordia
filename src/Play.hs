module Play where

import Control.Monad(when)
import Data.Maybe(fromMaybe)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Optics
import KOI.Basics
import KOI.Bag
import KOI
import Static
import State
import Types
import Constants
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

-- | Move the card at the given index from the hand to the top of the discard.
doDiscardCard :: PlayerId -> Int -> Interact ()
doDiscardCard pid n =
  do inHand <- the (playerState pid % playerHand)
     case splitAt n inHand of
       (as,b:bs) ->
         do setThe (playerState pid % playerHand) (as ++ bs)
            updateThe_ (playerState pid % playerDiscard) (b :)
       _ -> pure ()

-- | Pay for a worker and place it on the board
doBuildWorker :: PlayerId -> Worker -> CityId -> Interact ()
doBuildWorker pid w city =
  do sequence_
      [ updateThe_ (playerState pid % playerResources) (bagChange (-1) r)
      | r <- workerCost
      ]
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
         canPay =
           and [ bagContains r resources >= n
               | (r,n) <- bagToNumList (bagFromList workerCost) ]

countWorkersOnBoard :: PlayerId -> Interact Int
countWorkersOnBoard pid =
  do let count (p :-> _) = if p == pid then 1 else 0
     inCities <- the (board % mapCityWorkers)
     let cityCount = sum (sum . map count . bagToList <$> inCities )
     onPaths  <- the (board % mapPathWorkers)
     let pathCount = sum (count <$> onPaths)
     pure (cityCount + pathCount)



actTribune :: PlayerId -> Interact ()
actTribune pid =
  do discard <- updateThe (playerState pid % playerDiscard) (\d -> (d, []))
     doChangeMoney pid (length discard + 1 - 3)
     doAddCards pid discard
     ws <- canBuildWorker pid
     capital <- mapStartCity . mapLayout <$> the board
     askInputsMaybe_ "Would you like to build a worker?" $
        (pid :-> AskText "No", "Do not build worker.", pure ())
      : [ (pid :-> AskWorker w, "Build a worker.",
           doBuildWorker pid w capital)
        | w <- ws
        ]


actColonist :: PlayerId -> Int -> Interact ()
actColonist pid cardNum =
  do ws   <- canBuildWorker pid
     tgts <- getBuildTargets
     doAction tgts ws getMoney
     doDiscardCard pid cardNum

  where
  getBuildTargets =
    do b <- the board
       let ours = [ city | (city,ps) <- Map.toList (b ^. mapHouses)
                         , pid `elem` ps ]
       pure (mapStartCity (mapLayout b) : ours)

  getMoney =
    ( pid :-> AskText "Gain money."
    , "Gain money: 5 + deployed workers."
    , do n <- countWorkersOnBoard pid
         doChangeMoney pid (5 + n)
    )

  pass =
    ( pid :-> AskText "End turn."
    , "No more deployments."
    , pure ()
    )

  doAction tgts ws otherOpt =
     askInputsMaybe_ "Choose COLONIST actoin." $
       [ (pid :-> AskWorker w, "Deploy this worker.", buildWorker tgts w)
       | w <- ws ] ++ [otherOpt]

  buildWorker tgts w =
    askInputsMaybe_ "Choose where to deploy worker."
       [ ( pid :-> AskCity city
         , "Deploy here."
         , do doBuildWorker pid w city
              newWs <- canBuildWorker pid
              doAction tgts newWs pass
         )
       | city <- tgts ]



actPrefect :: PlayerId -> Int -> Interact ()
actPrefect pid cardNum =
  do brd <- the board
     let regs = Set.toList (mapRegions (mapLayout brd))
     askInputsMaybe_ "Choose a region to prefect."
        [ ( pid :-> AskRegion r
          , "Prefect this region."
          , do done <- the (board % mapPrefected)
               if r `elem` done then getMoney done else getGoods r
          )
        | r <- regs
        ]
     doDiscardCard pid cardNum
  where
  getMoney done =
    do bonuses <- the (board % mapRegionBonus)
       let fromRegion r =
             fromMaybe 0
             do bonus <- Map.lookup r bonuses
                pure (bonus ^. rbMoney)
       doChangeMoney pid (sum (map fromRegion done))
       setThe (board % mapPrefected) []

  getGoods r =
    do bonuses <- the (board % mapRegionBonus)
       undefined






