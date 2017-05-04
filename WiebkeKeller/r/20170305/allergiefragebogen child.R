setwd( "~/LIFE/github-tpeschel/life-for-postgraduates/WiebkeKeller/r/" )

source( "join.R" )
library( ggplot2 )

library(lubridate)
library(ggplot2)

## Bei wievielen Besuchen wurde eine laufende Nase diagnostiziert
table( tbl$S010293_F0012 )
summary( tbl$S010293_F0012 )

## Bei wievielen Besuchen wurde Heuschnupfen diagnostiziert
table( tbl$S010293_F0027 )
summary( tbl$S010293_F0027 )

## Bei wievielen Besuchen wurden Atembeschwerden diagnostiziert
table( tbl$S010293_F0039 )
summary( tbl$S010293_F0039 )

## Bei wievielen Besuchen wurde Asthma diagnostiziert?
table( tbl$S010293_F0041 )
summary( tbl$S010293_F0041 )

