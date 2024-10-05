module Main where

import Data.Map(Map)

import KOI.Basics
import KOI.CallJS(jsHandlers)
import KOI.Bag
import KOI.Options
import KOI.RNGM
import KOI
import Setup
import Play
import Constants
import Types
import Cards
import Maps


main :: IO ()
main = startApp App
  { appId = Concordia
  , appOptions = [ optBoard, optSalt, optForum, optWineMarket, optVenus ]
  , appColors = [ "yellow", "red", "green", "blue", "black" ]
  , appJS = $(jsHandlers [])
  , appInitialState = \rng opts ps ->
    do let cfg = getConfig opts ps
       pure (withRNG_ rng (setupGame cfg))
  , appStart = extraSetup >> play
  }



getConfig :: Options -> [PlayerId] -> Config
getConfig opts ps = 
  Config
    { cfgPlayers            = ps
    , cfgMapName            = getBoard opts
    , cfgMap                = mapData (getBoard opts)
    , cfgCityTiles          = mapTiles opts
    , cfgResourceLimit      = 12
    , cfgStartHireWorkers   = bagFromNumList [(Person,2), (Ship,2)]
    , cfgStartBoardWorkers  = bagFromList [Person,Ship]
    , cfgStartResources     = bagFromList [Brick,Wheat,if getSalt opts then Salt else Wheat,Tool,Wine,Cloth]
    , cfgStartMoney         = 5
    , cfgStartHouses        = 15
    , cfgMarket             = if useWineMarket opts then altMarketCosts else marketCosts
    , cfgPlayerCards        = if getVenus opts then startDeckVenus else startDeckBase
    , cfgMarketCards        = marketDeck (not (getVenus opts))
    , cfgUseSalt            = getSalt opts
    , cfgUseForum           = useForum opts
    }

-- Could move this to Setup?
mapTiles :: Options -> Map CityTile [Resource]
mapTiles opts
  | getSalt opts = if big then withTiles [(t,Salt)| t <- [A,B,C,D]] else cityTilesWithSalt
  | otherwise = if big then withTiles [(A,Tool),(B,Wine),(C,Cloth),(D,Brick)] else cityTiles
  where
  mp = getBoard opts
  big = mp == Hispania -- or byzantium


optSalt :: Option
getSalt :: Options -> Bool
(optSalt, getSalt) = flag "salt" (Just False) "Use salt"

optForum :: Option
useForum :: Options -> Bool
(optForum, useForum) = flag "forum" (Just False) "Use Forum tiles"

optVenus :: Option
getVenus :: Options -> Bool
(optVenus, getVenus) = flag "venus" (Just False) "Use Venus cards"

optWineMarket :: Option
useWineMarket :: Options -> Bool
(optWineMarket, useWineMarket) = flag "wine-market" (Just False) "Use wine market"

optBoard :: Option
getBoard :: Options -> Maps
(optBoard, getBoard) = option "board" (Just Italia) "Use this map"