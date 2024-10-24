module Play where

import Data.Text qualified as Text
import Control.Monad(when,unless,guard)
import Data.Maybe(fromMaybe,mapMaybe)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.List(foldl')
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
import Forum
import Log

extraSetup :: Interact ()
extraSetup =
  do fs <- the forumSetup
     when fs doForumSetup
     

doForumSetup :: Interact ()
doForumSetup =
  do pids <- Map.keys <$> the players  
     mapM_ drawTiles pids
     setThe (board % forumDiscard) =<< updateThe (board % forumMarket) (\xs -> (xs,[]))
     mapM_ pickTiles pids
     doRefillForumMarket
     setThe forumSetup False
  where
  drawTiles pid =
    do ours <- updateThe (board % forumMarket) (splitAt 2)
       setThe (playerState pid % playerForumTiles) (Set.fromList ours)
  pickTiles pid =
    do ours <- Set.toList <$> the (playerState pid % playerForumTiles)
       askInputsMaybe_ pid "Choose a patrician"
         [ (AskHandForum n, "Keep this patrician"
           , mapM_ (\x -> unless (x == t) (doDiscardForumTile pid x)) ours
           )
         | (n,t) <- zip [ 0 .. ] ours
         ]

play :: Interact ()
play =
  do s <- getState
     case s ^. gameStatus of
       Finished {} -> pure ()
       EndTriggeredBy pid
         | pid == s ^. curPlayer ->
          do let discard ps =
                   set playerHand [] $
                   over playerDiscard ((ps ^. playerHand) ++) ps
             updateThe_ players (fmap discard)
             doLog "Game Finished"
             setThe gameStatus (Finished pid)
             stop
         -- XXX: In team play we finish when it becomes our partner's turn.

         | otherwise -> doTakeTurn >> nextPlayer >> play
       InProgress -> doTakeTurn >> checkEndGame >> nextPlayer >> play

-- Assumes InProgress
checkEndGame :: Interact ()
checkEndGame =
  do s <- getState
     let playerDone p = p ^. playerHousesToBuild == 0
     when (null (s ^. board % marketDeck) || any playerDone (s ^. players))
       do let pid = s ^. curPlayer
          setThe gameStatus (EndTriggeredBy pid)
          -- Note in team play it might be an action of the team mate
          -- that triggered the end game, but we still attribute it to
          -- to the current player.  This doesn't matter because teams
          -- score jointly.
          doLogBy' pid [T "Triggered the end game"]


nextPlayer :: Interact ()
nextPlayer =
  do order <- the playerOrder
     updateThe_ curPlayer (playerAfter order)
     doLog' [L]

doTakeTurn :: Interact ()
doTakeTurn =
  do pid <- the curPlayer
     hand <- the (playerState pid % playerHand)
     askInputs pid "Choose an action"
       [ (AskHand n a, "Play " <> actionText act,
           do doDiscardCard pid n
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
    Diplomat -> actDiplomat
    Tribune -> actTribune
    Colonist co ->
      case co of
        Settle -> actColonistSettle
        Tax    -> actColonistTax
    Mercator n -> actMercator n
    Specialist r -> actSpecialist r
    Magister -> actMagister
    Consul -> actConsul []
    Legatus -> const (pure ()) -- XXX

actTribune :: PlayerId -> Interact ()
actTribune pid =
  do discard <- updateThe (playerState pid % playerDiscard) (\d -> (d, []))
     let actNum = length discard
     -- XXX: buy forum card
     changeSalt <- hasForumTile pid Titus
     when changeSalt (titusBonus pid)
  
     let amt = max 0 (actNum - 3)
     doChangeMoney pid amt
     doLogBy' pid [T "Gained", tSh amt, M, T "for Tribune"] 

     doAddCards pid discard
     (pays, ws) <- canBuildWorker pid
     capital <- the (board % mapLayout % mapStartCity)
     askInputsMaybe_ pid "Deploy a worker to the capital" $
      (AskText "End Turn", "Do not deploy worker", pure ()) : 
      [ (AskWorker w, "Deploy this worker", doBuildWorker pid w capital pays)
      | w <- ws
      ]


actColonistTax :: PlayerId -> Interact ()
actColonistTax pid =
  do n <- countWorkersOnBoard pid
     let amt = 5 + n
     doChangeMoney pid amt
     doLogBy' pid [T "Gained", tSh amt, M, T "for Colonist"]

actColonistSettle :: PlayerId -> Interact ()
actColonistSettle pid =
  do doLogBy pid "Settle Colonist"
     (pays,ws) <- canBuildWorker pid
     tgts <- colonistBuildTargets pid
     let tgtsFor = colonistWorkerTargets tgts
     let cityTgts = tgtsFor ws
     case (ws,cityTgts) of
       ([],_) -> doLogBy' pid [T "Has no workers they can afford"]
       (_,[]) -> doLogBy' pid [T "Has no cities they can deploy in"]
       _ -> pure ()
     doAction tgtsFor pays cityTgts pass
  where
  pass =
    ( AskText "End turn"
    , "No more deployments"
    , pure ()
    )

  doAction tgtsFor pays ws otherOpt =
     askInputsMaybe_ pid "Choose worker to deploy" $
       [ (AskWorker w, "Deploy worker", buildWorker tgtsFor wts pays)
       | wts@(w,_) <- ws ] ++ [otherOpt]

  buildWorker tgtsFor (w,cities) pays =
    askInputsMaybe_ pid "Choose deployment city"
       [ ( AskCity city
         , "Deploy here"
         , do doBuildWorker pid w city pays 
              (newPays,newWs) <- canBuildWorker pid
              doAction tgtsFor newPays (tgtsFor newWs) pass
         )
       | city <- cities ]



actPrefect :: PlayerId -> Interact ()
actPrefect pid =
  do regs <- the (board % mapLayout % mapRegions)
     sextus <- hasForumTile pid Sextus
     let extra =
           [ (AskText "Diplomat", "Sextus Pompeius", 
              do doLogBy' pid [T "Use as Diplomat (Sextus Pompeius)"]
                 actDiplomat pid)
           | sextus ]
     (done,moneyOpts) <- getMoneyOpt
     askInputsMaybe_ pid "Choose a province to Prefect" $
        moneyOpts ++ extra ++
        [ ( AskRegion r
          , "Prefect this province"
          , getGoods r
          )
        | r <- Set.toList regs, r `notElem` done
        ]
  where
  getMoneyOpt =
    do done <- the (board % mapPrefected)
       if null done then pure ([],[]) else
         do bonuses <- the (board % mapRegionBonus)
            let fromRegion r =
                  fromMaybe 0
                  do bonus <- Map.lookup r bonuses
                     pure (bonus ^. rbMoney)
            extra <- the (board % mapExtraMoneyBonus)
            let amt = sum (map fromRegion done) + extra
            let lab = Text.pack (show amt ++ "[Money]")
            pure
              ( done
              , [ ( AskText lab, "Prefect for money"
                  , do doChangeMoney pid amt
                       doLogBy' pid [T "Gained", tSh amt, M, T "from Prefect"] 
                       setThe (board % mapPrefected) []
                  )
                ])
            

  getPrefectBonus r =
    do rbs <- the (board % mapRegionBonus)
       case Map.lookup r rbs of
         Nothing -> pure mempty
         Just bonus ->
           case bonus ^. rbResource of
              NoBonus -> pure mempty
              VariableBonus ->
                askInputs pid "Choose bonus"
                  [ ( AskTextResource "Bonus" rsr
                    , "Prefect bonus"
                    , do let m = Map.findWithDefault 0 rsr resourcePrefectMoney
                         setThe (board % mapRegionBonus % ix r % rbMoney) m
                         resourceBonus rsr
                    )
                  | rsr <- normalResources
                  ]
              ResourceBonus rsr -> resourceBonus rsr
        where
        resourceBonus rsr =
          do pm <- the playerDoubleBonus
             amt' <- if pid == pm
                         then
                           do order <- the playerOrder
                              let newPM@(PlayerId who) = playerBefore order pid
                              setThe playerDoubleBonus newPM
                              doLog (who <> " is the new Prefctus Magnus")
                              pure 2
                         else pure 1
             donatus <- hasForumTile pid Donatus
             when donatus
               case Map.lookup rsr resourcePrefectMoney of
                 Just m ->
                   do updateThe_ (playerState pid % playerMoney) (+ m)
                      doLogBy' pid [T "Gained", tSh m, M, T "(Donatus Pompeius)"]
                 Nothing -> pure ()
             claudius <- hasForumTile pid Claudius
             amt <- if not claudius then pure amt' else
               let cost = Map.findWithDefault 0 rsr resourceCost in
               askInputs pid "Claudius Pompeius"
                 [ (AskText "Pass", "Don't sell a good", pure amt')
                 , ( AskTextResource "Sell" rsr, "Sell a bonus resource"
                   , do updateThe_ (playerState pid % playerMoney) (+ cost)
                        doLogBy' pid [T "Sold bonus", G rsr, T "(Claudius Pompeius)"]
                        pure (amt' - 1)
                   )
                 ]

             pure (Map.singleton pid (bagFromList (replicate amt rsr)))

  getGoods r =
    do doLogBy' pid [T "Prfect in", RID r]
       bonus <- getPrefectBonus r
       brd <- the board
       let cities = Map.findWithDefault [] r (citiesInRegion (brd ^. mapLayout))
       let doCity tot cid =
             fromMaybe tot
             do resource <- Map.lookup cid (brd ^. mapProduces)
                let earn = bagFromList [resource]
                houses <- Map.lookup cid (brd ^. mapHouses)
                let addP mp p = Map.insertWith bagUnion p earn mp
                pure (foldl' addP tot houses)
       mapM_ (uncurry (doGainResources Nothing)) 
             (Map.toList (foldl' doCity bonus cities))
       updateThe_ (board % mapPrefected) (r :)


actSpecialist :: Resource -> PlayerId -> Interact ()
actSpecialist r pid =
  do allHouses <- the (board % mapHouses)
     let ourCities = Map.keysSet (Map.filter (pid `elem`) allHouses)
     prod <- the (board % mapProduces)
     let thisResource = Map.keysSet (Map.filter (== r) prod)
     let amt = Set.size (Set.intersection ourCities thisResource)
     let name = specialistName r
     doGainResources (Just name) pid (bagFromNumList [(r,amt)])

actSenator :: PlayerId -> Interact ()
actSenator pid = 
  do doLogBy' pid [T "Senator"]
     cornelius <- hasForumTile pid Cornelius
     doPickCards pid 2 (if cornelius then ShopCornelius else ShopSenator) []

actConsul :: [Int] -> PlayerId -> Interact ()
actConsul ignore pid =
  do doLogBy' pid [T "Consul"]
     doPickCards pid 1 ShopConsul ignore

data BuildState = BuildState
  { archBuilt    :: Bool
  , archAnnelius :: Bool
  , archMamilius :: Bool
  , archMamericus :: Bool
  , archMarcus   :: Bool
  }

actArchitect :: PlayerId -> Interact ()
actArchitect pid =
  do doLogBy' pid [T "Architect"]
     doMoveWorkers pid
     buildHouses  
  where
  buildHouses =
    do ws <- doGetWorkers pid
       layout <- the (board % mapLayout)
       let cities = Set.fromList [ cid | (_,Right eid) <- ws
                                       , cid <- pathCities layout eid ]
           ours = [ (cid,False) | cid <- Set.toList cities ]
       prod   <- the (board % mapProduces)
       aulus  <- hasForumTile pid Aulus
       annaeus <- hasForumTile pid Annaeus
       adjPaths <- if not annaeus then pure [] else
         do allWs <- the (board % mapPathWorkers)
            let theirs = Set.fromList [ cid
                 | (eid, p :-> _) <- Map.toList allWs
                 , p /= pid
                 , let cids = pathCities layout eid
                 , cid <- cids
                 , not (cid `Set.member` cities)
                 , other <- filter (/= cid) cids
                 , other `Set.member` cities
                 ]
            pure [ (cid,True) | cid <- Set.toList theirs ]  
       let canBuild built cid =
             do houses <- case Map.lookup cid built of
                            Nothing -> pure 0
                            Just ps ->
                              do guard (pid `notElem` ps)
                                 pure (length ps)
                r             <- Map.lookup cid prod
                (money, cost) <- Map.lookup r cityCosts
                let baseCost = money * (1 + houses)
                pure (r, if aulus then (True, baseCost - 1) else (False,baseCost), cost)
      

       let cityOpts useAdj built =
              Map.toList $
              Map.fromListWith (&&)
              [ ((cid,cost),adj)
              | (cid,adj) <- if useAdj then adjPaths ++ ours else ours
              , Just cost <- [canBuild built cid]
              ]
       let s = BuildState { archBuilt = False, archAnnelius = annaeus
                          , archMarcus = False, archMamilius = False, archMamericus = False }
       buildHouse s cityOpts


  endBuild = (AskText "End Turn", "No more houses", pure ())


  checkCitizen grd tile act =
    do mb <- hasForumTileNum pid tile
       pure
        case mb of
           Nothing -> []
           Just n ->
             [ (AskHandForum n, "Use " <> Text.pack (show tile), doDiscardForumTile pid tile >> act)
             | grd ]

  buildHouse s cityOpts =
    do houses    <- the (playerState pid % playerHousesToBuild)
       built     <- the (board % mapHouses)
       resources <- the (playerState pid % playerResources)
       money     <- the (playerState pid % playerMoney)
       mamercusOpts   <- checkCitizen True Mamercus (buildHouse s { archMamericus = True } cityOpts)
       marcusOpts     <- checkCitizen True Marcus (buildHouse s { archMarcus = True } cityOpts)
       mamiliusOpts   <- checkCitizen (not (archBuilt s)) Mamilius
                            (buildHouse s { archMamilius = True } cityOpts)
       salt           <- the withSalt

       askInputs pid "Build houses" $
         endBuild :
         mamiliusOpts ++
         mamercusOpts ++
         marcusOpts ++
         [ ( AskCity cid
           , "Build house here."
           , do doChangeMoney pid (- moneyCost)
                doLogBy' pid ([T "Paid", tSh moneyCost, M] ++ [T "(Aulus Arcadius)"|discount])
                doPayCost pid cost
                updateThe_ (players % ix pid % playerHousesToBuild) (subtract 1)
                updateThe_ (board % mapHouses % at cid) (Just . (pid :) . fromMaybe [])
                doLogBy' pid ([T "Built in", CID cid] ++
                                            [T "(Annaeus Arcadius)" | useAdj])
                let s1 = BuildState
                           { archBuilt = True
                           , archMamilius = archMamilius s
                           , archMarcus = False
                           , archMamericus = False
                           , archAnnelius = archAnnelius s && not useAdj
                           }
                buildHouse s1 cityOpts
           )
         | houses >= 1
         , ((cid,(r, (discount,moneyCost'),rCost')),useAdj) <- cityOpts (archAnnelius s) built
         , not (archMarcus s) || not salt || r == Salt
         , let moneyCost = if archMamericus s || archMarcus s then 0 else moneyCost'
         , money >= moneyCost
         , let rCost'' | archMarcus s = [Tool]
                       | archMamilius s = filter (/= Brick) rCost'
                       | otherwise = rCost'
         , let rCost = map Resource rCost''
         , let cost = canAfford' rCost resources
         , not (null cost)
         ]



actMercator :: Int -> PlayerId -> Interact ()
actMercator n pid =
  do doChangeMoney pid n
     doLogBy' pid [ T "Gained", tSh n, M, T "from Mercator" ]
     extraTrade <- hasForumTile pid Servius
     freeBrick  <- hasForumTile pid Faustus
     bonus      <- hasForumTile pid Gaius
     let s = TradeS { tradeLimit = if extraTrade then 3 else 2
                    , freeBricks = if freeBrick then 1 else 0
                    , sellBonus  = if bonus then 1 else 0
                    , trades     = []
     }
     trade s

  where
  end = (AskText "End Turn", "Done trading", pure ())

  resources = Map.keys resourceCost

  trade as =
    do s <- the (playerState pid)
       askInputs pid "Trade" $
         end : cvtSalt as s ++
               mapMaybe (buyOpt as s) resources ++
               mapMaybe (sellOpt as s) resources  


  canBuy as s r = guard haveSpace >> haveMoney
    where
    used = bagSize (s ^. playerResources) + bagSize (s ^. playerWorkersForHire)
    haveSpace =  used < s ^. playerResourceLimit
    haveMoney
      | r == Brick && freeBricks as > 0 =
        pure (0, as { freeBricks = freeBricks as - 1 })
      | otherwise =
      do c <- Map.lookup r resourceCost
         guard (c <= s ^. playerMoney)
         pure (c,as)

  canSell s r =
    do c <- Map.lookup r resourceCost
       guard (bagContains r (s ^. playerResources) > 0)
       pure c

  cvtSalt as s =
    [ ( AskResource Salt, "Convert Salt"
      , askInputsMaybe_ pid "Convert Salt"
        [ (AskTextResource "To" r, "Convert to[" <> Text.pack (show r) <> "]",
           do updateThe_ (playerState pid % playerResources)
                         (bagChange (-1) Salt . bagChange 1 r)
              doLogBy' pid [T "Converted", G Salt, T "to", G r]
              trade as)
        | r <- resources
        ]
      )
    | bagContains Salt (s ^. playerResources) > 0
    ]

  doBuy as (r,c) =
    (AskTextResource "Buy" r, "Cost: " <> Text.pack (show c) <> "[Money]",
       do doChangeMoney pid (- c)
          updateThe_ (playerState pid % playerResources) (bagChange 1 r)
          doLogBy' pid [T "Bought", G r]
          trade as

    )

  doSell as (r,c) =
    (AskResource r, "Sell for " <> Text.pack (show c) <> "[Money]",
      do updateThe_ (playerState pid % playerResources) (bagChange (-1) r)
         doChangeMoney pid c
         doLogBy' pid [T "Sold", G r]
         trade as)
        

  actOk a@(_,r) as
    | a `elem` trades as = Just as  -- We already did this
    | r `elem` map snd (trades as) = Nothing -- Don't buy and sell the same thing
    | length (trades as) < tradeLimit as = Just as { trades = a : trades as }-- We have available action
    | otherwise = Nothing -- No more resource actions

  buyOpt as s r =
    do as' <- actOk (Buy,r) as
       (c,as'') <- canBuy as' s r
       pure (doBuy as'' (r,c))

  sellOpt as s r =
    do as' <- actOk (Sell,r) as
       c   <- canSell s r
       pure (doSell as' (r,c+sellBonus as))

data TradeS = TradeS
  { tradeLimit :: Int
  , freeBricks :: Int
  , sellBonus  :: Int
  , trades     :: [(TradeAct,Resource)]
  }

data TradeAct = Buy | Sell
  deriving Eq

actDiplomat :: PlayerId -> Interact ()
actDiplomat pid =
  do ps <- the players
     lucius <- hasForumTile pid Lucius
     extra <- if not lucius then pure [] else
       do n <- length <$> the (board % marketLayout)
          market <- take n <$> the (board % marketDeck)
          pure (concatMap getMarket (zip [0..] market))
     case extra ++ concatMap getTop (Map.toList ps) of
       [] -> doLogBy' pid [T "used Diplomat without an effect"] 
       opts -> askInputsMaybe_ pid "Choose action to copy" opts
  where
  getTop (i,s) =
    case s ^. playerDiscard of
      c : _
        | i /= pid ->
          [ (AskDiscard i n, "Perform this action",
              do doLogBy' pid [ T "Chose", T (actionText a), T "via Diplomat" ]
                 action a pid)
          | (n,a) <- zip [0..] (cardActions c), a /= Diplomat ] 
      _ -> []

  getMarket (i,c) =
    [ ( AskMarketAct i n, "Perform this action (Lucius Flavius)"
      , case a of
          Consul -> actConsul [i] pid
          _      -> action a pid
      )
    | (n,a) <- zip [0..] (cardActions c), a /= Diplomat
    ]

actMagister :: PlayerId -> Interact ()
actMagister pid =
  do cs <- the (playerState pid % playerDiscard)
     case cs of
       _ : c : _
         | let opts = [ ( AskText (actionText a), "Perform this action"
                        , action a pid )
                      | a <- cardActions c, a /= Senator && a /= Magister
                      ]
         , not (null opts) ->
          askInputsMaybe_ pid "Choose Magister action" opts 
       _ -> doLogBy' pid [T "used Magister without an effect"]