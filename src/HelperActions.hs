module HelperActions where

import Data.Text(Text)
import Data.Text qualified as Text
import Data.Maybe(fromMaybe)
import Data.List(nub,partition)
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
import Log



doLog' :: [LogWord] -> Interact ()
doLog' x = updateThe_ gameLog (x :)

doLog :: Text -> Interact ()
doLog x = doLog' [T x]

doLogBy :: PlayerId -> Text -> Interact ()
doLogBy pid txt = doLogBy' pid [T txt]

doLogBy' :: PlayerId -> [LogWord] -> Interact ()
doLogBy' (PlayerId p) ws = doLog' (T p : T ": " : ws)

doPrint :: Show a => a -> Interact ()
doPrint x = doLog (Text.pack (show x))

-- | Assumes the player is in the list of players.
playerAfter :: Eq a => [a] -> a -> a
playerAfter order pid =
  case dropWhile (/= pid) order of
    _ : next : _ -> next
    _            -> head order

-- | Assumes the player is in the list of players.
playerBefore :: Eq a => [a] -> a -> a
playerBefore order pid =
  case takeWhile (/= pid) order of
    [] -> last order
    xs -> last xs

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
     doLogBy' pid [T "Deployed", W (pid :-> w), T "to", CID city]

-- | Gain some reasource and ask which, if not enough space.
doGainResources :: Maybe Text -> PlayerId -> Bag Resource  -> Interact ()
doGainResources src pid new =
  do s <- the (playerState pid)
     let limit = s ^. playerResourceLimit
         haveR = bagSize (s ^. playerResources)
         haveW = bagSize (s ^. playerWorkersForHire)
         free  = limit - haveR - haveW
         need  = bagSize new

     let rs = case [ w | (r,n) <- bagToNumList new, w <- [ tSh n, G r ] ] of
                [] -> [T "nothing"]
                yes -> yes
     doLogBy' pid (T (fromMaybe "Gained" src) : rs)
     actual <- discard 1 (max 0 (need - free)) new
     updateThe_ (playerState pid % playerResources) (bagUnion actual) 
  where
  sh = Text.pack . show
  discard num total rs
    | num > total = pure rs
    | otherwise =
      fromMaybe bagEmpty <$>
      askInputsMaybe pid ("Choose a resource to discard (" <> sh num <> "/" <> sh total <> ")")
        [ (AskTextResource "Discard" r, "Discard resource.",
          do doLogBy' pid [T "Discarded", G r]
             discard (num+1) total (bagChange (-1) r rs)
          )
        | (r,_) <- bagToNumList rs
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
           askInputs pid "Pay resource"
              [ (AskResource r, "Pay with this.", doPick r)
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
         spotCosts = brd ^. marketLayout
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
       let num x = Text.pack (show x)
       let q = "Select a market card " <> num (length picked + 1)
               <> "/" <> num howMany
       askInputs pid q $
          ( AskText "End Turn", "No more cards.", pure picked) :
          [ ( AskMarket n, "Get this card."
            , do doPayCost pid opt
                 pickCard (n : picked) (filter ((/= n) . fst) avail)
            )
          | (n,opt) <- indexedOpts
          ]

-- | What paths can a worker reach with the given amount of work.
-- We don't count 0 movement.
canMoveWorker ::
  Worker -> Either CityId PathId -> Int -> Interact [(PathId,Int)]
canMoveWorker w loc0 steps =
  do layout  <- the (board % mapLayout)
     onPaths <- the (board % mapPathWorkers)
     let cityPaths = mapCityPaths w layout

         cityNeighbours cid = Map.findWithDefault [] cid cityPaths

         isBlocked eid =
            eid `Map.member` onPaths ||
            case Map.lookup eid (layout ^. mapPaths) of
              Just path -> not (path ^. pathCanStop)
              Nothing -> True

     let search visited reachable todo moreTodo =
           case todo of
             [] ->
               case moreTodo of
                 [] -> reachable
                 _  -> search visited reachable (reverse moreTodo) []
             (Left cid,stepsTaken) : rest ->
               let newWork = [ (Right loc, stepsTaken + 1)
                             | loc <- cityNeighbours cid
                             ]
               in search visited reachable rest (newWork ++ moreTodo)

             (Right loc,stepsTaken) : rest
               | loc `Set.member` visited || stepsTaken > steps ->
                 search visited reachable rest moreTodo
               | otherwise ->
                 let newVisted = Set.insert loc visited
                     newReachable =
                       if stepsTaken < 1 || isBlocked loc
                         then reachable
                         else (loc,stepsTaken) : reachable
                     newWork = [ (Left cid, stepsTaken)
                               | cid <- pathCities layout loc ]
                 in search newVisted newReachable todo (newWork ++ moreTodo)

     pure (search mempty mempty [(loc0,0)] [])




  