module Play where

import Control.Monad(when)
import Data.Maybe(fromMaybe)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.List(foldl')
import Optics
import KOI.Basics
import KOI.Bag
import KOI
import Static
import State
import Types
import Question
import HelperActions

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
  do order <- the playerOrder
     updateThe_ curPlayer (playerAfter order)

doTakeTurn :: Interact ()
doTakeTurn = pure ()


actTribune :: PlayerId -> Interact ()
actTribune pid =
  do discard <- updateThe (playerState pid % playerDiscard) (\d -> (d, []))
     doChangeMoney pid (length discard - 3)
     doAddCards pid discard
     ws <- canBuildWorker pid
     capital <- mapStartCity . mapLayout <$> the board
     askInputsMaybe_ "Would you like to build a worker?" $
        (pid :-> AskText "End turn.", "Do not build worker.", pure ())
      : [ (pid :-> AskWorker w, "Build a worker.",
           doBuildWorker pid w capital)
        | w <- ws
        ]


actColonist :: PlayerId -> Interact ()
actColonist pid =
  do ws   <- canBuildWorker pid
     tgts <- getBuildTargets
     doAction tgts ws getMoney
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



actPrefect :: PlayerId -> Interact ()
actPrefect pid =
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
  where
  getMoney done =
    do bonuses <- the (board % mapRegionBonus)
       let fromRegion r =
             fromMaybe 0
             do bonus <- Map.lookup r bonuses
                pure (bonus ^. rbMoney)
       doChangeMoney pid (sum (map fromRegion done))
       setThe (board % mapPrefected) []

  getPlayerBefore =
    do order <- the playerOrder
       pure (playerBefore order pid)

  getPrefectBonus r =
    do rbs <- the (board % mapRegionBonus)
       case view rbResource =<< Map.lookup r rbs of
         Nothing -> pure mempty
         Just rsr ->
           do pm <- the playerDoubleBonus
              amt <- if pid == pm
                          then do p <- getPlayerBefore
                                  setThe playerDoubleBonus p
                                  pure 2
                          else pure 1
              pure (Map.singleton pid (bagFromList (replicate amt rsr)))

  getGoods r =
    do bonus <- getPrefectBonus r
       brd <- the board
       let cities = Map.findWithDefault [] r (citiesInRegion (mapLayout brd))
       let doCity tot cid =
             fromMaybe tot
             do resource <- Map.lookup cid (brd ^. mapProduces)
                let earn = bagFromList [resource]
                houses <- Map.lookup cid (brd ^. mapHouses)
                let addP mp p = Map.insertWith bagUnion p earn mp
                pure (foldl' addP tot houses)
       mapM_ (uncurry doGainResources) (Map.toList (foldl' doCity bonus cities))


actSpecialist :: PlayerId -> Resource -> Interact ()
actSpecialist pid r =
  do allHouses <- the (board % mapHouses)
     let ourCities = Map.keysSet (Map.filter (pid `elem`) allHouses)
     prod <- the (board % mapProduces)
     let thisResource = Map.keysSet (Map.filter (== r) prod)
     let amt = Set.size (Set.intersection ourCities thisResource)
     doGainResources pid (bagFromNumList [(r,amt)])


actSenator :: PlayerId -> Interact ()
actSenator pid = doPickCards pid 2 True

actConsul :: PlayerId -> Interact ()
actConsul pid = doPickCards pid 1 False






