module Maps.Italia where

import Data.Map qualified as Map
import Types
import Maps.Builder

definition = build
  do region "Venetia"
     bavzanvm <- city "Bavzanvm" A
     aqvileia <- city "Aqvileia" A
     verona   <- city "Verona" A

     region "Transpadana"
     comvm   <- city "Comvm" A
     segvsio <- city "Segviso" A

     region "Ligvria"
     nicea <- city "Nicea" A
     genva <- city "Genva" A

     region "Emilia"
     mvtina <- city "Mvtina" B
     ravenna <- city "Ravenna" B

     region "Etrvria"
     florentia <- city "Florentia" B
     cosa <- city "Cosa" B

     region "Corsica"
     aleria <- city "Aleria" B
     olbia <- city "Olbia" B

     region "Vmbria"
     ancona <- city "Ancona" C
     hadria <- city "Hadria" C
     spoletvm <- city "Spoletvm" C

     region "Campania"
     roma <- city "Roma" Capital
     casinvm <- city "Casinvm" B
     neapolis <- city "neapolis" B

     region "Apvlia"
     lvceria <- city "Lvceria" C
     brvndisivm <- city "Brvundisivm" C

     region "Lvcania"
     potentia <- city "Potentia" C
     croton <- city "Croton" C

     region "Sicilia"
     panormvs <- city "Panromvs" C
     messana <- city "Messana" C
     syracvse <- city "Syracvse" C


     landPath brvndisivm aqvileia
     landPath brvndisivm verona
     landPath aqvileia verona
     landPath aqvileia ravenna
     waterPath aqvileia ravenna
     waterPath aqvileia ancona
     landPath verona ravenna
     landPath verona comvm
     landPath verona mvtina

     landPath comvm mvtina
     landPath comvm segvsio
     landPath comvm genva
     landPath segvsio nicea

     landPath genva florentia
     landPath genva nicea
     waterPath genva cosa
     waterPath genva aleria
     waterPath genva nicea
     waterPath nicea aleria

     -- XXX: Region 4...





