module Main where

import Data.Aeson qualified as JS
import KOI.Basics
import KOI.Bag
import KOI.RNGM
import KOI
import Setup
import Play
import Constants
import Types
import Cards
import Static

main :: IO ()
main =
  -- XXX
  JS.eitherDecodeFileStrict' "ui/maps/Italia.json" >>= \mb ->
  case mb of
    Left err -> fail err
    Right mp ->
      startApp App
      { appId = Concordia
      , appOptions = []
      , appColors = [ "yellow", "red", "green", "blue", "black" ]
      , appJS = ""
      , appInitialState = \rng opts ps ->
        withRNG_ rng
          do s <- setupGame (getConfig mp opts ps)
             pure (Right s)
      , appStart = play
      }



getConfig :: MapLayout -> config -> [PlayerId] -> Config
getConfig mp _opts ps = Config
  { cfgPlayerOrder        = ps
  , cfgMap                = mp
  , cfgCityTiles          = cityTiles
  , cfgResourceLimit      = 12
  , cfgStartHireWorkers   = bagFromNumList [(Person,2), (Ship,2)]
  , cfgStartBoardWorkers  = bagFromList [Person,Ship]
  , cfgStartResources     = bagFromList [Brick,Wheat,Wheat,Tool,Wine,Cloth]
  , cfgStartMoney         = 5
  , cfgStartHouses        = 15
  , cfgMarket             = marketCosts
  , cfgPlayerCards        = startDeck
  , cfgMarketCards        = marketDeck
  }




