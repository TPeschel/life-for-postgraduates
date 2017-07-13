#setwd( "../R/Doktorarbeit/" )

rm( list = ls( ) )

#library( hlpr4life )

#load.pkgs( c( "dplyr", "ggplot2", "readxl", "lubridate", "Hmisc" ) )

setwd( "~/LIFE/life-for-postgraduates/WiebkeKeller/data/" )

load( "../results/joinWithPsych.Rd" )

library( dplyr )
library( ggplot2 )
library( readxl )
library( lubridate )
library( Hmisc )

sapply( as.data.frame( !is.na( tbl[ , grepl( "EDAT", names( tbl ) ) ] ) ), sum )
sapply( as.data.frame( is.na( tbl[ , grepl( "EDAT", names( tbl ) ) ] ) ), sum )

names( tbl )[ names( tbl ) == "TEILNEHMER_SIC" ] <- "SIC"
names( tbl )[ names( tbl ) == "sex" ] <- "SEX"
names( tbl )[ names( tbl ) == "C_DISEASE_TX_EDAT" ] <- "EDAT"

tbl%<>%
    group_by( SIC ) %>%
    mutate( visit = dense_rank( EDAT ) )

tbl <-
    tbl[ tbl$visit == 1, ]

tbl.psy.allerg <-
    tbl[ !is.na( tbl$C_DISEASE_TX_PSY ) & !is.na( tbl$C_DISEASE_TX_ALLERG ), ]

tbl.psy.knd <-
    tbl.psy.allerg %>%
    group_by( SIC, SEX ) %>%
    summarise( 
        score.psy    = max( C_DISEASE_TX_PSY ),
        score.allerg = max( C_DISEASE_TX_ALLERG ) )

table( tbl.psy.knd[ , c( "score.psy", "score.allerg", "SEX" ) ] )

tbl.psy.knd.male <- tbl.psy.knd[ tbl.psy.knd$SEX == "male", ]
tbl.psy.knd.female <- tbl.psy.knd[ tbl.psy.knd$SEX == "female", ]

( glm.male <-
    glm( data = tbl.psy.knd.male , score.psy ~ score.allerg, family = binomial( "logit" ) ) )

summary( glm.male )

exp( coef( glm.male ) )

rcorr( tbl.psy.knd.male$score.psy, tbl.psy.knd.male$score.allerg )


( glm.female <-
    glm( data = tbl.psy.knd.female, score.psy ~ score.allerg, family = binomial( "logit" ) ) )

summary( glm.female )

exp( coef( glm.female ) )

rcorr( tbl.psy.knd.female$score.psy, tbl.psy.knd.female$score.allerg )

ggplot( tbl.psy.allerg ) +
    geom_jitter( aes( C_DISEASE_TX_PSY, C_DISEASE_TX_ALLERG, col = SEX ) )

# polychoric correlation
# x is a contingency table of counts

load.pkgs("polycor")

polychor( tbl.psy.allerg ) 

# heterogeneous correlations in one matrix 
# pearson (numeric-numeric), 
# polyserial (numeric-ordinal), 
# and polychoric (ordinal-ordinal)
# x is a data frame with ordered factors 
# and numeric variables
library(polycor)
hetcor(x) 


# partial correlations
load.pkgs( "ggm" )

data( tbl.psy.allerg )

pcor(c("a", "b", "x", "y", "z"), var(tbl.psy.allerg))
# partial corr between a and b controlling for x, y, z
