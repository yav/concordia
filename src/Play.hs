module Play where

import Control.Monad(when)
import Data.Maybe(fromMaybe)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.List(foldl',nub,partition)
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

-- | Gain some reasource and ask which, if not enough space.
doGainResources :: PlayerId -> Bag Resource -> Interact ()
doGainResources pid new =
  do s <- the (playerState pid)
     let limit = s ^. playerResourceLimit
         haveR = bagSize (s ^. playerResources)
         haveW = bagSize (s ^. playerWorkersForHire)
         free  = limit - haveR - haveW
         need  = bagSize new
     if need <= free
       then updateThe_ (playerState pid % playerResources) (bagUnion new)
       else askWhich new free
  where
  askWhich todo free
    | free == 0 = pure ()
    | otherwise =
      askInputsMaybe_ "Choose resource to gain."
        [ (pid :-> AskResource r, "Gain resource.",
          do updateThe_ (playerState pid % playerResources) (bagChange 1 r)
             askWhich (bagChange (-1) r todo) (free - 1)
          )
        | (r,_) <- bagToNumList todo
        ]

-- | What kind of worker can this player build at the moment
-- (i.e., they have the worker and the resources to build it).
canBuildWorker :: PlayerId -> Interact [Worker]
canBuildWorker pid =
  do pstate <- the (playerState pid)
     let workers   = pstate ^. playerWorkersForHire
         hasW p    = bagContains p workers > 0
         resources = pstate ^. playerResources
         canPay =
           and [ bagContains r resources >= n
               | (r,n) <- bagToNumList (bagFromList workerCost) ]
     pure [ w | canPay, w <- [Person,Ship], hasW w ]

-- | Compute various ways in which a player can pay a cost.
-- If the result is empty, then they can't affor this.
canAfford :: PlayerId -> [ResourceCost] -> Interact [Bag Resource]
canAfford pid cost =
  do resources <- the (playerState pid % playerResources)
     pure (canAfford' cost resources)

-- | Compute various ways to satisfy a cost using some resources.
canAfford' :: [ResourceCost] -> Bag Resource -> [Bag Resource]
canAfford' need0 have0 = nub (map bagFromList ans)
  where
  ans = go need0 have0
  go need have =
    case need of
      [] -> pure []
      x : xs ->
        case x of
          Any ->
            do (r,_) <- bagToNumList have
               rs <- go xs (bagChange (-1) r have)
               pure (r:rs)
          Resource r ->
            do pay <- [r,Salt]
               case bagChangeMaybe (-1) pay have of
                 Just have' ->
                   do rs <- go xs have'
                      pure (pay : rs)
                 Nothing -> []

-- | Choose one of the ways to pay for the given cost.
-- The list option should be non-empty (i.e., they can afford it)
doPayCost :: PlayerId -> [Bag Resource] -> Interact ()
doPayCost pid opts0
  | null opts0 = pure ()  -- they can't afford it, give it for free :-)
  | otherwise = go opts0
  where
  go opts
    | any bagIsEmpty opts = pure ()
    | otherwise =
    do let allRs = nub [ r | opt <- opts, (r,_) <- bagToNumList opt ]
           inAll r = all ((> 0) . bagContains r) opts
           doPick r =
             do updateThe_ (playerState pid % playerResources)
                           (bagChange (-1) r)
                go [ b1
                   | opt <- opts
                   , Just b1 <- [bagChangeMaybe (-1) r opt]
                   ]
       case filter inAll allRs of
         r : _ -> doPick r
         [] ->
           askInputs "How would would like to pay?"
              [ (pid :-> AskResource r, "Pay with this.", doPick r)
              | r <- allRs
              ]


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
        (pid :-> AskText "End turn.", "Do not build worker.", pure ())
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

  playerBefore =
    do before <- the prevPlayers
       after  <- the nextPlayers
       cur    <- the curPlayer
       pure $ last
            $ takeWhile (/= pid)
            $ cycle
            $ after ++ reverse before ++ [cur]

  getPrefectBonus r =
    do rbs <- the (board % mapRegionBonus)
       case view rbResource =<< Map.lookup r rbs of
         Nothing -> pure mempty
         Just rsr ->
           do pm <- the playerDoubleBonus
              amt <- if pid == pm
                          then do p <- playerBefore
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

actSpecialist :: PlayerId -> Resource -> Int -> Interact ()
actSpecialist pid r cardId =
  do allHouses <- the (board % mapHouses)
     let ourCities = Map.keysSet (Map.filter (pid `elem`) allHouses)
     prod <- the (board % mapProduces)
     let thisResource = Map.keysSet (Map.filter (== r) prod)
     let amt = Set.size (Set.intersection ourCities thisResource)
     doGainResources pid (bagFromNumList [(r,amt)])
     doDiscardCard pid cardId


actSenator :: PlayerId -> Int -> Interact ()
actSenator pid cardNum =
  do brd <- the board
     let cards = brd ^. marketDeck
         spotCosts = marketLayout brd
         marketLen = length spotCosts
         cost c b = map Resource (cardCost c) ++ b
         avail = zip [0..] (zipWith cost cards spotCosts)
     picked <- pickCard [] avail
     let (pickedCards, otherCards) =
            partition ((`elem` picked) . fst)
                      (zip (take marketLen [ 0 .. ]) cards)
     doAddCards pid (map snd pickedCards)
     setThe (board % marketDeck)
            (map snd otherCards ++ drop marketLen cards)
     doDiscardCard pid cardNum
  where
  pickCard picked avail
    | length picked >= 2 = pure picked
    | otherwise =
    do opts <- mapM (canAfford pid . snd) avail
       let indexedOpts = [ (n,opt)
                         | ((n,_),opt) <- zip avail opts
                         , not (null opt)
                         ]
       askInputs "Select a card." $
          ( pid :-> AskText "End Turn", "No more cards.", pure picked) :
          [ ( pid :-> AskMarket n, "Get this card."
            , do doPayCost pid opt
                 pickCard (n : picked) (filter ((/= n) . fst) avail)
            )
          | (n,opt) <- indexedOpts
          ]


