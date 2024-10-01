module Play where

import Data.Text qualified as Text
import Control.Monad(when,forM,guard)
import Data.Maybe(fromMaybe,catMaybes,mapMaybe)
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
    Consul -> actConsul
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
     (pays,ws)   <- canBuildWorker pid
     tgts <- getBuildTargets
     case (ws,tgts) of
       ([],_) -> doLogBy' pid [T "Has on workers they can afford"]
       (_,[]) -> doLogBy' pid [T "Has no cities they can deploy in"]
       _ -> pure ()
     doAction tgts pays ws pass
  where
  getBuildTargets =
    do brd <- the board
       let capital = brd ^. mapLayout % mapStartCity
       let ours = [ city | (city,ps) <- Map.toList (brd ^. mapHouses)
                         , pid `elem` ps ]
       pure (capital : ours)

  pass =
    ( AskText "End turn"
    , "No more deployments"
    , pure ()
    )

  doAction tgts pays ws otherOpt =
     askInputsMaybe_ pid "Choose worker to deploy" $
       [ (AskWorker w, "Deploy worker", buildWorker tgts w pays)
       | w <- ws ] ++ [otherOpt]

  buildWorker tgts w pays =
    do paths <- mapCityPaths w <$> the (board % mapLayout)
       let suitable = not . null . flip (Map.findWithDefault []) paths
       askInputsMaybe_ pid "Choose deployment city"
          [ ( AskCity city
            , "Deploy here"
            , do doBuildWorker pid w city pays 
                 (newPays,newWs) <- canBuildWorker pid
                 doAction tgts newPays newWs pass
            )
          | city <- tgts, suitable city ]



actPrefect :: PlayerId -> Interact ()
actPrefect pid =
  do regs <- the (board % mapLayout % mapRegions)
     askInputsMaybe_ pid "Choose a province to Prefect"
        [ ( AskRegion r
          , "Prefect this province"
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
       extra <- the (board % mapExtraMoneyBonus)
       let amt = sum (map fromRegion done) + extra
       doChangeMoney pid amt
       doLogBy' pid [T "Gained", tSh amt, M, T "from Prefect"] 
       setThe (board % mapPrefected) []

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
                  | rsr <- [ Brick, Wheat, Tool, Wine, Cloth ]
                  ]
              ResourceBonus rsr -> resourceBonus rsr
        where
        resourceBonus rsr =
          do pm <- the playerDoubleBonus
             amt <- if pid == pm
                         then
                           do order <- the playerOrder
                              setThe playerDoubleBonus (playerBefore order pid)
                              pure 2
                         else pure 1
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
     doPickCards pid 2 True

actConsul :: PlayerId -> Interact ()
actConsul pid =
  do doLogBy' pid [T "Consul"]
     doPickCards pid 1 False

actArchitect :: PlayerId -> Interact ()
actArchitect pid =
  do doLogBy' pid [T "Architect"]
     move' <- countWorkersOnBoard pid
     yes <- hasForumTile pid Appius
     let move = if yes then move' + 1 else move'
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

  endMove = (AskText "Build", "No more moves", pure ())

  moveCityWorker steps w cid tgts =
    ( AskCityWorker cid w
    , "Move this worker"
    , askInputs pid "Move to"
        [ ( AskPath eid
          , "Move here"
          , do updateThe_ (board % mapCityWorkers % ix cid)
                          (bagChange (-1) (pid :-> w))
               setThe (board % mapPathWorkers % at eid) (Just (pid :-> w))
               doLogBy' pid [T "Moved", W (pid :-> w), T "from", CID cid, T "to", PID eid]
               doMoves (steps - takenSteps)
          )
        | (eid, takenSteps) <- tgts
        ]
    )

  movePathWorker steps w from tgts =
    ( AskPath from
    , "Move this worker"
    , askInputs pid "Move to"
        [ ( AskPath tgt
          , "Move here"
          , do setThe (board % mapPathWorkers % at from) Nothing
               setThe (board % mapPathWorkers % at tgt) (Just (pid :-> w))
               doLogBy' pid [T "Moved", W (pid :-> w), T "from", PID from, T "to", PID tgt]
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

       askInputsMaybe_ pid "Move worker" $
         endMove :
         [ case loc of
             Left cid  -> moveCityWorker steps w cid tgts
             Right eid -> movePathWorker steps w eid tgts
         | (w,loc,tgts) <- opts
         ]

  buildHouses =
    do ws <- getWorkers
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
                pure (if aulus then baseCost - 1 else baseCost, map Resource cost)
      

       let cityOpts useAdj built =
              Map.toList $
              Map.fromListWith (&&)
              [ ((cid,cost),adj)
              | (cid,adj) <- if useAdj then adjPaths ++ ours else ours
              , Just cost <- [canBuild built cid]
              ]
       buildHouse annaeus cityOpts


  endBuild = (AskText "End Turn", "No more houses", pure ())

  buildHouse adj cityOpts =
    do houses    <- the (playerState pid % playerHousesToBuild)
       built     <- the (board % mapHouses)
       resources <- the (playerState pid % playerResources)
       money     <- the (playerState pid % playerMoney)

       askInputs pid "Build houses" $
         endBuild :
         [ ( AskCity cid
           , "Build house here."
           , do doChangeMoney pid (- moneyCost)
                doLogBy' pid [T "Paid", tSh moneyCost, M]
                doPayCost pid cost
                updateThe_ (players % ix pid % playerHousesToBuild) (subtract 1)
                updateThe_ (board % mapHouses % at cid) (Just . (pid :) . fromMaybe [])
                doLogBy' pid ([T "Built in", CID cid] ++
                                            [T "(Annaeus Arcadius)" | useAdj])
                buildHouse (adj && not useAdj) cityOpts
           )
         | houses >= 1
         , ((cid,(moneyCost,rCost)),useAdj) <- cityOpts adj built
         , money >= moneyCost
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
     case concatMap getTop (Map.toList ps) of
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