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
     segvsio <- city "Segvsio" A

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
     neapolis <- city "Neapolis" B

     region "Apvlia"
     lvceria <- city "Lvceria" C
     brvndisivm <- city "Brvndisivm" C

     region "Lvcania"
     potentia <- city "Potentia" C
     croton <- city "Croton" C

     region "Sicilia"
     panormvs <- city "Panormvs" C
     messana <- city "Messana" C
     syracvse <- city "Syracvse" C


     landPath bavzanvm aqvileia
     landPath bavzanvm verona
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
     landPath segvsio genva

     landPath genva florentia
     landPath genva nicea
     waterPath genva cosa
     waterPath genva aleria
     waterPath genva nicea
     waterPath nicea aleria

     landPath mvtina florentia
     landPath mvtina ravenna
     landPath ravenna ancona
     landPath ravenna florentia
     waterPath ravenna ancona

     landPath florentia spoletvm
     landPath florentia cosa
     landPath cosa roma
     waterPath cosa aleria
     waterPath cosa roma

     waterPath aleria olbia
     waterPath olbia roma
     waterPath olbia panormvs

     landPath ancona hadria
     landPath ancona spoletvm
     landPath spoletvm hadria
     landPath spoletvm roma
     landPath spoletvm casinvm
     landPath hadria casinvm
     landPath hadria lvceria
     waterPath ancona hadria
     waterPath hadria brvndisivm

     landPath roma casinvm
     landPath casinvm neapolis
     landPath casinvm lvceria
     landPath neapolis potentia
     waterPath roma panormvs
     waterPath roma neapolis
     waterPath neapolis panormvs
     waterPath neapolis messana

     landPath lvceria brvndisivm
     landPath lvceria potentia
     landPath brvndisivm potentia
     waterPath brvndisivm croton

     landPath potentia croton
     landPath croton messana
     waterPath croton messana
     waterPath croton syracvse

     landPath messana panormvs
     landPath messana syracvse
     landPath panormvs syracvse
     waterPath messana panormvs
     waterPath messana syracvse


