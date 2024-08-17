module HelperActions where

import Data.Maybe(fromMaybe)
import Data.List(nub,partition)
import Optics
import KOI.Basics
import KOI.Bag
import KOI
import Static
import State
import Types
import Constants
import Question

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


doGetMarketCards :: Bool -> Interact [(Card, [ResourceCost])]
doGetMarketCards withBoardCost =
  do brd <- the board
     let cards = brd ^. marketDeck
         spotCosts = marketLayout brd
         cost c b =
           (c, map Resource (cardCost c) ++ if withBoardCost then b else [])
     pure (zipWith cost cards spotCosts)

doPickCards :: PlayerId -> Int -> Bool -> Interact ()
doPickCards pid howMany extraCost =
  do avail <- zip [ 0 .. ] <$> doGetMarketCards extraCost
     let marketLen = length avail
     picked <- pickCard []  avail
     let (pickedCards, otherCards) =
            partition ((`elem` picked) . fst) avail
     doAddCards pid (map (fst . snd) pickedCards)
     updateThe_ (board % marketDeck)
                ((map (fst . snd) otherCards ++) . drop marketLen)
  where
  pickCard picked avail
    | length picked >= howMany = pure picked
    | otherwise =
    do opts <- mapM (canAfford pid . snd . snd) avail
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







