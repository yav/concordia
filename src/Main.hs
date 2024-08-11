module Main where

import KOI.Basics
import KOI.RNGM
import KOI
import Setup
import Play

main :: IO ()
main =
  startApp App
  { appId = Concordia
  , appOptions = []
  , appColors = [ "yellow", "red", "green", "blue", "black" ]
  , appJS = ""
  , appInitialState = \rng opts ps ->
    withRNG_ rng
      do s <- setupGame (getConfig opts ps)
         pure (Right s)
  , appStart = play
  }



getConfig :: config -> [PlayerId] -> Config
getConfig _opts ps = Config
  { cfgPlayerOrder        = ps
  , cfgMap                = undefined
  , cfgCityTiles          = undefined
  , cfgResourceLimit      = undefined
  , cfgStartHireWorkers   = undefined
  , cfgStartBoardWorkers  = undefined
  , cfgStartResources     = undefined
  , cfgStartMoney         = undefined
  , cfgStartHouses        = undefined

  , cfgMarket             = undefined
  , cfgPlayerCards        = undefined
  , cfgMarketCards        = undefined
  }




