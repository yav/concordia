module Main where

import Data.Aeson qualified as JS
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


main :: IO ()
main = startApp App
  { appId = Concordia
  , appOptions = [ optMap ]
  , appColors = [ "yellow", "red", "green", "blue", "black" ]
  , appJS = $(jsHandlers [])
  , appInitialState = \rng opts ps ->
    do cfg <- getConfig opts ps
       pure (withRNG_ rng (setupGame cfg))
  , appStart = play
  }



getConfig :: Options -> [PlayerId] -> IO Config
getConfig opts ps = 
  do mp <- do mb <- JS.eitherDecodeFileStrict' (getMap opts)
              case mb of
                Left err -> fail err
                Right a  -> pure a
     pure Config
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
       , cfgPlayerCards        = startDeckVenus
       , cfgMarketCards        = marketDeck False
       }



optMap :: Option
getMap :: Options -> String
(optMap, getMap) = optionString "map" "PATH" Nothing "Path to map.json"
