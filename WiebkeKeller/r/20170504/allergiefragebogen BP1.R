setwd( "~/LIFE/github-tpeschel/life-for-postgraduates/WiebkeKeller/r/20170504/" )

source( "join.R" )
library( ggplot2 )

library(lubridate)
library(ggplot2)

## Bei wievielen Besuchen wurde eine laufende Nase diagnostiziert
table( tbl$FB_ALLERGY_BP1_F0014 )
summary( tbl$FB_ALLERGY_BP1_F0014 )

## Bei wievielen Besuchen wurde Heuschnupfen diagnostiziert
table( tbl$FB_ALLERGY_BP1_F0029 )
summary( tbl$FB_ALLERGY_BP1_F0029 )

## Bei wievielen Besuchen wurden Atembeschwerden diagnostiziert
table( tbl$FB_ALLERGY_BP1_F0041 )
summary( tbl$FB_ALLERGY_BP1_F0041 )

## Bei wievielen Besuchen wurde Asthma diagnostiziert?
table( tbl$FB_ALLERGY_BP1_F0043 )
summary( tbl$FB_ALLERGY_BP1_F0043 )
