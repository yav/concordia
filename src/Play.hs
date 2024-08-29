module Play where

import Control.Monad(when,forM,guard)
import Data.Maybe(fromMaybe,catMaybes)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.List(nub,foldl')
import Optics
import KOI.Basics
import KOI.Bag
import KOI
import Constants
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

-- XXX
doTakeTurn :: Interact ()
doTakeTurn =
  do pid@(PlayerId txt)  <- the curPlayer
     hand <- the (playerState pid % playerHand)
     askInputs (txt <> ": Choose a card action")
       [ (pid :-> AskHand n a, "Play this card",
           do doDiscardCard pid n
              sync
              action act pid)
       | (n,c) <- zip [ 0 .. ] hand
       , (a,act) <- zip [0..] (cardActions c)
       ]


action :: Action -> PlayerId ->  Interact ()
action act =
  case act of
    Architect -> actArchitect
    Senator -> actSenator
    Prefect -> actPrefect
    Diplomat -> const (pure ()) -- XXX
    Tribune -> actTribune
    Colonist co ->
      case co of
        Settle -> actColonistSettle
        Tax    -> actColonistTax
    Mercator _n -> const (pure ()) -- XXX
    Specialist r -> actSpecialist r
    Magister -> const (pure ()) -- XX
    Consul -> actConsul

actTribune :: PlayerId -> Interact ()
actTribune pid@(PlayerId name) =
  do discard <- updateThe (playerState pid % playerDiscard) (\d -> (d, []))
     doChangeMoney pid (max 0 (length discard - 3))
     doAddCards pid discard
     sync
     ws <- canBuildWorker pid
     capital <- the (board % mapLayout % mapStartCity)
     askInputsMaybe_ (name <> ": Deploy a worker?") $
      (pid :-> AskText "End Turn", "Do not deploy worker.", pure ())
      : [ (pid :-> AskWorker w, "Deploy this worker.",
           doBuildWorker pid w capital)
        | w <- ws
        ]


actColonistTax :: PlayerId -> Interact ()
actColonistTax pid =
  do n <- countWorkersOnBoard pid
     doChangeMoney pid (5 + n)
     sync


actColonistSettle :: PlayerId -> Interact ()
actColonistSettle pid =
  do ws   <- canBuildWorker pid
     tgts <- getBuildTargets
     doAction tgts ws pass
  where
  getBuildTargets =
    do brd <- the board
       let capital = brd ^. mapLayout % mapStartCity
       let ours = [ city | (city,ps) <- Map.toList (brd ^. mapHouses)
                         , pid `elem` ps ]
       pure (capital : ours)

  pass =
    ( pid :-> AskText "End turn."
    , "No more deployments."
    , pure ()
    )

  doAction tgts ws otherOpt =
     askInputsMaybe_ "Choose a worker to deploy" $
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
  do regs <- the (board % mapLayout % mapRegions)
     askInputsMaybe_ "Choose a region to prefect."
        [ ( pid :-> AskRegion r
          , "Prefect this region."
          , do done <- the (board % mapPrefected)
               if r `elem` done then getMoney done else getGoods r
          )
        | r <- Set.toList regs
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

  getPrefectBonus r =
    do rbs <- the (board % mapRegionBonus)
       case view rbResource =<< Map.lookup r rbs of
         Nothing -> pure mempty
         Just rsr ->
           do pm <- the playerDoubleBonus
              amt <- if pid == pm
                          then
                            do order <- the playerOrder
                               setThe playerDoubleBonus (playerBefore order pid)
                               pure 2
                          else pure 1
              pure (Map.singleton pid (bagFromList (replicate amt rsr)))

  getGoods r =
    do bonus <- getPrefectBonus r
       brd <- the board
       let cities = Map.findWithDefault [] r (citiesInRegion (brd ^. mapLayout))
       let doCity tot cid =
             fromMaybe tot
             do resource <- Map.lookup cid (brd ^. mapProduces)
                let earn = bagFromList [resource]
                houses <- Map.lookup cid (brd ^. mapHouses)
                let addP mp p = Map.insertWith bagUnion p earn mp
                pure (foldl' addP tot houses)
       mapM_ (uncurry doGainResources) (Map.toList (foldl' doCity bonus cities))


actSpecialist :: Resource -> PlayerId -> Interact ()
actSpecialist r pid =
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

actArchitect :: PlayerId -> Interact ()
actArchitect pid@(PlayerId name) =
  do move <- countWorkersOnBoard pid
     doMoves move
     buildHouses

  where
  getWorkers =
    do brd <- the board
       let fromCity cid inCity rest =
             [ (w, Left cid) | w <- [Person,Ship]
                             , bagContains (pid :-> w) inCity > 0 ] ++
             rest
       let cities = Map.foldrWithKey fromCity [] (brd ^. mapCityWorkers)

       let fromPath eid (p :-> w) rest
             | pid == p  = (w,Right eid) : rest
             | otherwise = rest
       pure (Map.foldrWithKey fromPath cities (brd ^. mapPathWorkers))

  endMove = (pid :-> AskText "Build", "No more moves", pure ())

  moveCityWorker steps w cid tgts =
    ( pid :-> AskCityWorker cid w
    , "Move this worker"
    , askInputs (name <> ": Move to")
        [ ( pid :-> AskPath eid
          , "Move here"
          , do updateThe_ (board % mapCityWorkers % ix cid)
                          (bagChange (-1) (pid :-> w))
               setThe (board % mapPathWorkers % at eid) (Just (pid :-> w))
               sync
               doMoves (steps - takenSteps)
          )
        | (eid, takenSteps) <- tgts
        ]
    )

  movePathWorker steps w from tgts =
    ( pid :-> AskPath from
    , "Move this worker"
    , askInputs (name <> ": Move to")
        [ ( pid :-> AskPath tgt
          , "Move here"
          , do setThe (board % mapPathWorkers % at from) Nothing
               setThe (board % mapPathWorkers % at tgt) (Just (pid :-> w))
               sync
               doMoves (steps - takenSteps)
          )
        | (tgt, takenSteps) <- tgts
        ]
    )

  doMoves steps =
    do ws <- getWorkers
       opts <- catMaybes <$> forM ws \(w,loc) ->
         do opts <- canMoveWorker w loc steps
            if null opts
              then pure Nothing
              else pure (Just (w,loc,opts))

       askInputsMaybe_ (name <> ": Move worker") $
         endMove :
         [ case loc of
             Left cid  -> moveCityWorker steps w cid tgts
             Right eid -> movePathWorker steps w eid tgts
         | (w,loc,tgts) <- opts
         ]

  buildHouses =
    do ws <- getWorkers
       let paths = [ eid | (_,Right eid) <- ws ]
       layout <- the (board % mapLayout)
       prod   <- the (board % mapProduces)
       let canBuild built cid =
             do houses <- case Map.lookup cid built of
                            Nothing -> pure 0
                            Just ps ->
                              do guard (pid `notElem` ps)
                                 pure (length ps)
                r             <- Map.lookup cid prod
                (money, cost) <- Map.lookup r cityCosts
                pure (money * (1 + houses), map Resource cost)
       let cityOpts built =
              nub [ (cid,cost)
                  | eid <- paths
                  , cid <- pathCities layout eid
                  , Just cost <- [canBuild built cid]
                  ]
       buildHouse cityOpts


  endBuild = (pid :-> AskText "End Turn", "No more houses.", pure ())

  buildHouse cityOpts =
    do houses    <- the (playerState pid % playerHousesToBuild)
       built     <- the (board % mapHouses)
       resources <- the (playerState pid % playerResources)
       money     <- the (playerState pid % playerMoney)

       askInputs "Build houses." $
         endBuild :
         [ ( pid :-> AskCity cid
           , "Build house here."
           , do doChangeMoney pid (- moneyCost)
                doPayCost pid cost
                updateThe_ (players % ix pid % playerHousesToBuild) (subtract 1)
                updateThe_ (board % mapHouses % at cid) (Just . (pid :) . fromMaybe [])
                sync
                buildHouse cityOpts
           )
         | houses >= 1
         , (cid,(moneyCost,rCost)) <- cityOpts built
         , money >= moneyCost
         , let cost = canAfford' rCost resources
         , not (null cost)
         ]


