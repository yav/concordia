module Main where

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
  , appOptions = [ optBoard, optSalt, optVenus ]
  , appColors = [ "yellow", "red", "green", "blue", "black" ]
  , appJS = $(jsHandlers [])
  , appInitialState = \rng opts ps ->
    do let cfg = getConfig opts ps
       pure (withRNG_ rng (setupGame cfg))
  , appStart = play
  }



getConfig :: Options -> [PlayerId] -> Config
getConfig opts ps = 
  Config
    { cfgPlayerOrder        = ps
    , cfgMapName            = getBoard opts
    , cfgMap                = mapData (getBoard opts)
    , cfgCityTiles          = if getSalt opts then cityTilesWithSalt
                                              else cityTiles
    , cfgResourceLimit      = 12
    , cfgStartHireWorkers   = bagFromNumList [(Person,2), (Ship,2)]
    , cfgStartBoardWorkers  = bagFromList [Person,Ship]
    , cfgStartResources     = bagFromList [Brick,Wheat,if getSalt opts then Salt else Wheat,Tool,Wine,Cloth]
    , cfgStartMoney         = 5
    , cfgStartHouses        = 15
    , cfgMarket             = marketCosts
    , cfgPlayerCards        = if getVenus opts then startDeckVenus else startDeckBase
    , cfgMarketCards        = marketDeck (not (getVenus opts))
    }


optSalt :: Option
getSalt :: Options -> Bool
(optSalt, getSalt) = flag "salt" (Just False) "Use salt"

optVenus :: Option
getVenus :: Options -> Bool
(optVenus, getVenus) = flag "venus" (Just False) "Use Venus cards"

optBoard :: Option
getBoard :: Options -> Maps
(optBoard, getBoard) = option "board" (Just Italia) "Use this map"